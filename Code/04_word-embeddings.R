# Word Embeddings ---------------------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)
library(widyr)
library(furrr)

# Import nested word tokens.
word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))

word_tokens

word_counts <- word_tokens |> 
  unnest(cols = words) |> 
  count(word)

custom_stop_words <- word_counts |> 
  # arrange(desc(n)) |> 
  # mutate(id = row_number()) |> 
  # summarize(
  #   q1 = 257915 / 2, 
  #   q2 = 257915,
  #   q3 = 257915 + (length(257915:nrow(word_counts)) / 2)
  # )
  arrange(desc(n)) |>
  # slice(round(257915 / 2):nrow(word_counts)) |>
  slice(1000:nrow(word_counts)) |>
  select(word)

word_tokens <- word_tokens |> 
  unnest(words) |> 
  anti_join(custom_stop_words, by = "word") |> 
  group_by(id) |> 
  nest(words = c(word)) |> 
  ungroup()
  
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

# window_size set to "default" 4, let's try something smaller
# nv set to "default"

###########

# Try removing words that aren't used often to make this work?
# - Removing words less than...1000 occurrences?!
# Need to parallelize AND split to make this work?

# tidy_pmi <- word_tokens |> 
  mutate(words = future_map(words, slide_windows, 2L)) |> 

tidy_pmi <- tidy_pmi |> 
  unnest(words) |> 
  unite(window_id, id, window_id) |> 
  pairwise_pmi(word, window_id)

tidy_pmi |> 
  slice(1) |> 
  unnest(words) |> 
  select(id, year, quarter, word, window_id) |> 
  unite(window_id, id, window_id) |> 
  pairwise_pmi(word, window_id)


# Creating the vectors
tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

# # Hurray!

# Save word_embeddings...

