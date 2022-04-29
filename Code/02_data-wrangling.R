# Tokenizing --------------------------------------------------------------
# Load packages
# Need widyr, furrr, slider, and irlba.
library(tidyverse)
library(tidytext)
library(widyr)
library(furrr)
library(irlba)
library(stopwords)
library(edgar)

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

# Tokens

# Need to deal with contractions ('ll, 've, 't, 's, 'd, 're)
# Replaced contractions with proper words, may need to add more in the future
# cont <- list("ll", "ve", "t", "s", "d", "re")
# 'd can be had or would

word_tokens <- call_data %>%
  mutate(text= str_replace_all(text, "\\.", " ")) %>%
  mutate(text= str_replace_all(text, "\\b", " ")) %>%
  mutate(text= str_replace_all(text, "'ll", " will")) %>% 
  mutate(text= str_replace_all(text, "'ve", " have")) %>% 
  mutate(text= str_replace_all(text, "'t", " not")) %>% 
  mutate(text= str_replace_all(text, "'d", " had")) %>% 
  mutate(text= str_replace_all(text, "'s", " is")) %>% 
  mutate(text= str_replace_all(text, "'re", " are")) %>% 
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

# SMART, Snowball, and onix are all included within Tidytext, thorough
# review should be done of each to see which we should include. The 
# Loughran McDonald stopword list will probably be the best, as it is domain
# specific. The `stopwords` package has SMART and Snowball, as well as ISO

# Snowball
word_tokens_snowball <- word_tokens %>%
  filter(!(word %in% stopwords(source = "snowball")))

# SMART
word_tokens_smart <- word_tokens %>%
  filter(!(word %in% stopwords(source = "smart")))

# ISO
word_tokens_iso <- word_tokens %>%
  filter(!(word %in% stopwords(source = "stopwords-iso")))

# onix
onix <- stop_words %>% filter(lexicon == "onix") %>% select(word)

word_tokens_onix <- word_tokens %>%
  anti_join(onix)

# All lists
word_tokens_all <- word_tokens_lm  %>% 
  anti_join(stop_words) %>% 
  filter(!(word %in% stopwords(source = "stopwords-iso")))



# Business/Marketing Dictionary -------------------------------------------

# edgar package has the LM dictionary, which is a good place to start
# Column values indicate the years words were added to the list
data(LMMasterDictionary)
LM <- as_tibble(LMMasterDictionary)


# Mutate to binary
LM_binary <- LM %>%
  mutate(negative = if_else(negative != 0, 1, 0),
         positive = if_else(positive != 0, 1, 0),
         uncertainty = if_else(uncertainty != 0, 1, 0),
         litigious = if_else(litigious != 0, 1, 0),
         strong_modal = if_else(modal != 1, 0, 1),
         moderate_modal = if_else(modal != 2, 0, 1),
         weak_modal = if_else(modal != 3, 0, 1)) %>%
  select(word, negative, positive, uncertainty, litigious, strong_modal,
         moderate_modal, weak_modal)



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
# I won't use the stemmed words for this, unless I find a compelling reason to.
# No stopwords removed, contractions are gone though

# Add counts, filter by 10 or up, nest words 
nested_words <- word_tokens %>% 
  add_count(word) %>%
  filter(n >= 10) %>%
  select(-n) %>%
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

# COmmenting out for now, will revisit later

# tidy_pmi <- nested_words %>%
#   mutate(words = future_map(words, slide_windows, 8L)) %>%
#   unnest(words) %>%
#   unite(window_id, title, window_id) %>%
#   pairwise_pmi(word, window_id)


# Creating the vectors

# tidy_word_vectors <- tidy_pmi %>%
#   widely_svd(
#     item1, item2, pmi,
#     nv = 100, maxit = 1000
#   )
# 
# # Hurray!


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


# nearest_neighbors(tidy_word_vectors, "q1")


# Document embeddings
# Incorrect dimensions when not filtering stopwords
# word_matrix <- word_tokens %>%
#   count(title, word) %>%
#   cast_sparse(title, word, n)
# 
# embedding_matrix <- tidy_word_vectors %>%
#   cast_sparse(item1, dimension, value)
# 
# doc_matrix <- word_matrix %*% embedding_matrix
# 
# dim(doc_matrix)
