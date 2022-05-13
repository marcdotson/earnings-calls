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
  mutate(text = str_replace_all(text, "\\.", " ")) %>%
  mutate(text = str_replace_all(text, "\\b", " ")) %>%
  mutate(text = str_replace_all(text, "(?<=')ll", " will")) %>%
  mutate(text = str_replace_all(text, "(?<=')ve ", "have")) %>%
  mutate(text = str_replace_all(text, "(?<=')t ", " not")) %>%
  mutate(text = str_replace_all(text, "(?<=')d ", " had")) %>%
  mutate(text = str_replace_all(text, "(?<=')s ", " is")) %>%
  mutate(text = str_replace_all(text, "(?<=')re ", " are")) %>%
  unnest_tokens(word, text, token = "words") %>% 
  mutate(word = if_else(word == "ve", "have", word)) %>% 
  mutate(word = if_else(word == "ll", "will", word))

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
sw_names <- read_csv(here::here("Data", "stopwords_lm_names.csv")) %>% 
  distinct() %>% 
  filter(word != "SALE" |
         word != "SALES" |
         word != "APPLE" |
         word != "BANE" |
         word != "BATTLE" |
         word != "BATTLES" |
         word != "BEER" |
         word != "BEERS" |
         word != "BEST" |
         word != "BIAS" |
         word != "BLAND" |
         word != "BLANK" |
         word != "BLISS" |
         word != "BLIZZARD" |
         word != "BLOCK" |
         word != "BLOOM" |
         word != "BLOSSOM" |
         word != "BLOW" |
         word != "BLUNT" |
         word != "BOARD" |
         word != "BODE" |
         word != "BOSS" |
         word != "BOTTOMS" |
         word != "BOX" |
         word != "BRAND" |
         word != "BRANCH" |
         word != "BRINK" |
         word != "BUTTON" |
         word != "BURGER" |
         word != "CALL" |
         word != "CANDY" |
         word != "CARD" | 
         word != "CARRY" |
           word != "CASE" |
           word != "BRINK" |
           word != "COFFEE" |
           word != "CONSTANT" |
           word != "CASH" |
           word != "CHAMPION" |
           word != "CHANCE" |
           word != "CHARITY" |
           word != "CHASE" |
           word != "CHRISTMAS" |
           word != "CLICK" |
           word != "CLOSE" |
           word != "CLOUD" |
           word != "COATS" |
           word != "COFFEE" |
           word != "COFFIN" |
           word != "COOL" |
           word != "COPE" |
           word != "CORE" |
           word != "COTTON" |
           word != "COUCH" |
           word != "COVER" |
           word != "CONSTANT" |
           word != "CRYSTAL" |
           word != "DAY" |
           word != "DEAL" |
           word != "DENT" |
           word != "DO" |
           word != "DOLLAR" |
           word != "DOT" |
           word != "DOVE" |
           word != "DRAIN" |
           word != "DRUM" |
           word != "EASTER" |
           word != "FAIR" |
           word != "FAST" |
           word != "FIELD" |
           word != "FINE" |
           word != "FISH" |
           word != "FIX" |
           word != "FORTUNE" |
           word != "FRAME" |
           word != "FREE" |
           word != "FREED" |
           word != "FRIES" |
           word != "FRIEND" |
           word != "FRY" |
           word != "GAMBLE" |
           word != "GOLD" |
           word != "GOLDEN" |
           word != "GOOD" |
           word != "GREEN" |
           word != "GRIM" |
           word != "GROSS" |
           word != "GUESS" |
           word != "HAIR" |
           word != "HAND" |
           word != "HANG" |
           word != "HARDER" |
           word != "HARNESS" |
           word != "HARMS" |
           word != "HASTY" |
           word != "HEAD" |
           word != "HEARD" |
           word != "HIGH" |
           word != "HOLDER" |
           word != "HONEY" |
           word != "HOPE" |
           word != "HOUSE" |
           word != "HOOK" |
           word != "JOY" |
           word != "JUST" |
           word != "KEY" |
           word != "KEYS" |
           word != "KIT" |
           word != "LAND" |
           word != "LARGE" |
           word != "LAW" |
           word != "LAWS" |
           word != "LAWYER" |
           word != "LAY" |
           word != "LEAK" |
           word != "LEAN" |
           word != "LENT" |
           word != "LIGHT" |
           word != "LINK" |
           word != "LITTLE" |
           word != "LOCK" |
           word != "LOAN" |
           word != "LONG" |
           word != "LOVE" |
           word != "LOVING" |
           word != "LOW" |
           word != "MAJOR" |
           word != "MAN" |
           word != "MANUAL" |
           word != "MASK" |
           word != "MAY" |
           word != "MEANS" |
           word != "MERCY" |
           word != "MIX" |
           word != "MOCK" |
           word != "MONEY" |
           word != "MORE" |
           word != "MUSIC" |
           word != "NAIL" |
           word != "NATION" |
           word != "NATIONS" |
           word != "NEW" |
           word != "PACK" |
           word != "PASS" |
           word != "PATCH" |
           word != "PEACE" |
           word != "PEAK" |
           word != "PEEK" |
           word != "POOL" |
           word != "PRECIOUS" |
           word != "PRICE" |
           word != "PRIOR" |
           word != "PRUDENCE" |
           word != "QUICK" |
           word != "RASH" |
           word != "REAL" |
           word != "RICH" |
           word != "RING" |
           word != "SAMPLE" |
           word != "SAMPLES" |
           word != "SEAL" |
           word != "SEALS" |
           word != "SEE" |
           word != "SELL" |
           word != "SELLS" |
           word != "SELLERS" |
           word != "SETTLE" |
           word != "SETTLES" |
           word != "SMALL" |
           word != "SONG" |
           word != "SPEAKS" |
           word != "SPEED" |
           word != "STILL" |
           word != "STOCK" |
           word != "STOCKS" |
           word != "STREET" |
           word != "TINY" |
           word != "TOY" |
           word != "TREAT" |
           word != "WALL" |
           word != "WAY" |
           word != "WEEKS" |
           word != "WILL" |
           word != "WISDOM" |
           word != "WISE" |
           word != "WORTH" |
           word != "WORD" 
        ) %>% add_row(word = "OPPENHEIMER")
# names includes sales, going to remove it for now

# hand picked marketing terms
terms <- read_csv(here::here("Data", "marketing_words.csv")) %>% 
  mutate(word= str_to_lower(word))


lm_stopwords_list <- sw_auditor %>% 
  bind_rows(sw_dates_numbers, sw_generic_long, sw_geography, sw_names) %>% 
  mutate(word= str_to_lower(word))
  

# Loughran McDonald stop words
word_tokens_lm <- word_tokens %>% anti_join(lm_stopwords_list) 

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


# Word counts after filtering with different stop word lexicons

# No stop words
no_stopwords <- word_tokens %>%
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "No stop words")

# Loughran McDonald stop words
lm_stopwords <- word_tokens_lm %>% 
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "Loughran McDonald")

# Tidytext stop words (SMART, Snowball, ISO)
tidytext_stopwords <- word_tokens_tt %>% 
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "tidytext")

# SMART stop words
smart_stopwords <- word_tokens_smart %>% 
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "SMART")

# Snowball stop words
snowball_stopwords <- word_tokens_snowball %>% 
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "Snowball")

# ISO stop words
iso_stopwords <- word_tokens_iso %>% 
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "ISO")

# onix stop words
onix_stopwords <- word_tokens_onix %>% 
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "onix")

# All stop word lexicons
all_stopwords <- word_tokens_all %>% 
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "All stop word lexicons")

marketing_words <- word_tokens %>% 
  inner_join(terms) %>% 
  group_by(year) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "Marketing Words")

# Binding previous data frames, grouped by year and quarter
word_counts <- no_stopwords %>% 
  bind_rows(tidytext_stopwords,
            lm_stopwords,
            smart_stopwords,
            iso_stopwords,
            snowball_stopwords,
            onix_stopwords,
            all_stopwords,
            marketing_words)

write_csv(word_counts, here::here("Data", "word_counts.csv"))

# Word totals, grouped by word/stop word
word_tot <- word_counts %>%
  ungroup() %>% 
  select(word, n, stopwords) %>% 
  group_by(word, stopwords) %>% 
  summarise(totals = sum(n))


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
  select(word, negative, positive) #, uncertainty, litigious, strong_modal,
     #    moderate_modal, weak_modal)

# Revenue and sentiment for Apple, not scalable at the moment. Sentiment
# is the number of positive terms subtracted from negative, so some information
# is lost here. LM tokens were used to match the sentiment list
revenue_sentiment <- word_tokens_lm %>% 
  group_by(year, quarter) %>% 
  left_join(LM_binary, by = "word") %>% 
  mutate(negative = replace_na(negative, 0),
         positive = replace_na(positive, 0)) %>% 
  summarise(revenue=revenue,
            sentiment_difference = (sum(positive)-sum(negative))) %>% 
  distinct() %>% 
  ungroup %>% 
  mutate(period = row_number()) #unsure how to do this exactly, will fix later


write_csv(revenue_sentiment, here::here("Data", "revenue_sentiment_apple.csv") )


revenue_sentiment %>% 
  ggplot(aes(x = period, y = sentiment_difference)) +
  geom_col()
  
  
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
nested_words <- word_tokens_lm %>% 
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


nearest_neighbors(tidy_word_vectors, "sales")

nearest_neighbors(tidy_word_vectors, "drive")

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
