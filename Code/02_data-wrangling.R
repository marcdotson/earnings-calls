# Tokenizing --------------------------------------------------------------
# Load packages
# Need widyr, furrr, slider, and irlba.
library(tidyverse)
library(tidytext)
library(widyr)
library(furrr)
library(irlba)


# Read data.
call_data <- read_rds(here::here("Data", "call_data.rds"))
joint_data <- read_rds(here::here("Data", "joint_data.rds"))

#Notes on packages:
#The SMLTA book incorporates several different packages which supplement the tidytext package:
#tokenizer is, surprise surprise, a tokenizer
#stopwords package, which may or may not already be a part of tidytext
#SnowballC does stemming
#quanteda creates and manages sparse matrices
#slider creates slide windows; used for word embeddings
#widyr does word embeddings; supposedly this package isn't good for academic purposes
#furrr enables parallel processing
#text2vec, word2vec, and FastText provide alternative options for embeddings
#edgar has the LMMasterDictionary

# We need to tokenize based on what? Some things to consider:
# - You may want to include title and text together.
# - Documents defined by earnings call or year/quarter or year.
# - Keep all the group information as you tokenize.

# Los Tokens

word_tokens <- call_data %>%
  mutate(text= str_replace_all(text, "\\.", " ")) %>%
  mutate(text= str_replace_all(text, "\\b", " ")) %>%
  unnest_tokens(word, text, token = "words")

# Remove Stop Words -------------------------------------------------------
# - stopwords data frame needs to be looked at in tidytext.
# - specialized stopwords .txt file provided by the dictionary authors.

# Import LM Stopword lists, generic is contained within generic_long

sw_auditor <- read_csv(here::here("Data", "stopwords_lm_auditor.csv"))
# sw_currency <- read_csv(here::here("Data", "stopwords_lm_currency.csv"))
sw_dates_numbers <- read_csv(here::here("Data", "stopwords_lm_dates_numbers.csv"))
sw_generic_long <- read_csv(here::here("Data", "stopwords_lm_generic_long.csv"))
# sw_generic <- read_csv(here::here("Data", "stopwords_lm_generic.csv"))
sw_geography <- read_csv(here::here("Data", "stopwords_lm_geography.csv"))
sw_names <- read_csv(here::here("Data", "stopwords_lm_names.csv"))

lm_stopwords <- sw_auditor %>% 
  bind_rows(sw_dates_numbers, sw_generic_long, sw_geography, sw_names) %>% 
  mutate(word= str_to_lower(word))
  

# Loughran McDonald stop words
word_tokens_lm <- word_tokens %>% anti_join(lm_stopwords)

# Tidytext stop words
word_tokens_tt <- word_tokens %>% anti_join(stop_words)

# Both lists
word_tokens_both <- word_tokens_lm  %>% anti_join(stop_words)


wordcount <- word_tokens_both %>% count(word, sort= TRUE)

# Need to deal with contractions ('ll, 've, 't, 's, 'd)
# I'll just remove them for now

cont <- list("ll", "ve", "t", "s", "d")

word_tokens_both <- word_tokens_both %>% filter(!(word %in% cont))

# Stemming ----------------------------------------------------------------

# SMLTA Book isn't a big proponent of stemming and offers some published
# evidence that it can even be counterproductive. Still, I'll include code,
# but it may not be used

# Using SnowballC
stem_tokens_snowball <- word_tokens_both %>% 
  mutate(stems = SnowballC::wordStem(word))

# Using hunspell--note how it drops essentially everything that isn't a word.
# Probably not a good call for what we're doing
stem_tokens_hunspell <- word_tokens_both %>%
  mutate(stems = hunspell::hunspell_stem(word))

# Word Embeddings ---------------------------------------------------------

# Word embeddings seem to occupy a space somewhere between data wrangling and
# modeling. They are computationally intensive and require a fair bit of
# judgment. The process used to make word embeddings is outlined in SMLTA
# I won't use the stemmed words for this, unless I find a compelling reason to

# Add counts, filter by 10 or up 
word_tokens_both <- word_tokens_both %>% 
  add_count(word) %>%
  filter(n >= 10) %>%
  select(-n)

# Nest the words
nested_words <- word_tokens_both %>%
  nest(words = c(word))

# Creating slide windows, which are used to calculate skipgram probabilities


slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, 
    ~.x, 
    .after = window_size - 1, 
    .step = 1, 
    .complete = TRUE
  )
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))
  
  out %>%
    transpose() %>%
    pluck("result") %>%
    compact() %>%
    bind_rows()
}


plan(multisession)  ## for parallel processing

# Not exactly sure how to explain what I'm doing with this, which isn't great
# 8L for 8 cores! Didn't take long to run though, even if you have 4 cores

tidy_pmi <- nested_words %>%
  mutate(words = future_map(words, slide_windows, 8L)) %>%
  unnest(words) %>%
  unite(window_id, title, window_id) %>%
  pairwise_pmi(word, window_id)


# Creating the vectors

tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

# Hurray!


# Function from SMLTA to find the nearest neighbor of words

nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) / 
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) %>%
    select(-item2)
}


nearest_neighbors(tidy_word_vectors, "q1")


# Document embeddings

word_matrix <- word_tokens_both %>%
  count(title, word) %>%
  cast_sparse(title, word, n)

embedding_matrix <- tidy_word_vectors %>%
  cast_sparse(item1, dimension, value)

doc_matrix <- word_matrix %*% embedding_matrix

dim(doc_matrix)
