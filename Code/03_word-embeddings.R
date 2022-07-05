# Word Embeddings ---------------------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)
library(widyr)
library(furrr)

# Import nested word tokens.
word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))

word_tokens

##########
tidy_complaints <- complaints %>%
  select(complaint_id, consumer_complaint_narrative) %>%
  unnest_tokens(word, consumer_complaint_narrative) %>%
  add_count(word) %>%
  filter(n >= 50) %>%
  select(-n)

nested_words <- tidy_complaints %>%
  nest(words = c(word))

nested_words
##########




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

# window_size
# nv

tidy_pmi <- word_tokens %>%
  mutate(words = future_map(words, slide_windows, 4L)) %>%
  unnest(words) %>%
  unite(window_id, rowID, window_id) %>%
  pairwise_pmi(word, window_id)


# Creating the vectors
tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

# # Hurray!

# Save word_embeddings...

