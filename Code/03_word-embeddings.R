# Word Embeddings ---------------------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)
library(widyr)
library(furrr)

#slider creates slide windows; used for word embeddings
#widyr does word embeddings; supposedly this package isn't good for academic purposes
#furrr enables parallel processing
#text2vec, word2vec, and FastText provide alternative options for embeddings

# Word embeddings seem to occupy a space somewhere between data wrangling and
# modeling. They are computationally intensive and require a fair bit of
# judgment. The process used to make word embeddings is outlined in SMLTA
# I won't use the stemmed words for this, unless I find a compelling reason to.
# No stopwords removed, contractions are gone though

# Add counts, filter by 10 or up, nest words 

call_tokens <- read_csv(here::here("Data", "call_tokens.csv"))

call_tokens <- call_tokens %>% 
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

tidy_pmi <- call_tokens %>%
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


# # Function from SMLTA to find the nearest neighbor of words
# 
# nearest_neighbors <- function(df, token) {
#   df %>%
#     widely(
#       ~ {
#         y <- .[rep(token, nrow(.)), ]
#         res <- rowSums(. * y) / 
#           (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
#         
#         matrix(res, ncol = 1, dimnames = list(x = names(res)))
#       },
#       sort = TRUE
#     )(item1, dimension, value) %>%
#     select(-item2)
# }
# 
# 
# nearest_neighbors(tidy_word_vectors, "sales")
# 
# nearest_neighbors(tidy_word_vectors, "drive")
