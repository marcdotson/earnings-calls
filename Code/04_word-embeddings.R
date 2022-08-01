# Word Embeddings ---------------------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)
library(widyr)
library(furrr)
# Import nested word tokens.
word_tokens <- read_rds(here::here("Data", "word_tokens.rds")) |> select(id, words)

word_tokens 

word_dict <- word_tokens |> 
  unnest(cols = words) |> 
  count(word) |> 
  filter(n >= 50)

word_dict <- word_dict |> 
  mutate(word_ind = as.character(seq(1, nrow(word_dict)))) |> 
  select(-n)

write_rds(word_dict, here::here("Data", "word_dict"))

word_tokens_hashed <- word_tokens |> 
  unnest(cols = words) |>
  semi_join(word_dict) |> 
  left_join(word_dict) |> 
  select(id, word_ind)


# 6.7 GB
write_rds(word_tokens_hashed,  here::here("Data", "word_tokens_hashed.rds"))


word_tokens_unhashed <- word_tokens |> 
  left_join(word_dict) |> 
  select(id, word)


# 7.4 GB
write_rds(word_tokens_unhashed,  here::here("Data", "word_tokens_unhashed.rds"))

word_tokens_int_hashed <- word_tokens |> 
  mutate(word_ind = as.integer(word_ind))

# 3.2 GB
write_rds(word_tokens_int_hashed,  here::here("Data", "word_tokens_int_hashed.rds"))

word_tokens_int_hashed <- read_rds(here::here("Data", "word_tokens_int_hashed.rds"))
# custom_stop_words <- word_counts |> 
  # arrange(desc(n)) |> 
  # mutate(id = row_number()) |> 
  # summarize(
  #   q1 = 257915 / 2, 
  #   q2 = 257915,
  #   q3 = 257915 + (length(257915:nrow(word_counts)) / 2)
  # )
#   arrange(desc(n)) |>
#   # slice(round(257915 / 2):nrow(word_counts)) |>
#   slice(1000:nrow(word_counts)) |>
#   select(word)
# 
# word_tokens <- word_tokens |> 
#   unnest(words) |> 
#   anti_join(custom_stop_words, by = "word") |> 
#   group_by(id) |> 
#   nest(words = c(word)) |> 
#   ungroup()
#   
# word_tokens

##########
# tidy_complaints <- complaints %>%
#   select(complaint_id, consumer_complaint_narrative) %>%
#   unnest_tokens(word, consumer_complaint_narrative) %>%
#   add_count(word) %>%
#   filter(n >= 50) %>%
#   select(-n)
# 
# nested_words <- tidy_complaints %>%
#   nest(words = c(word))
# 
# nested_words
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

temp1 <- word_tokens_int_hashed |> 
  filter(id <= 1000) |> 
  count(word_ind) |> 
  filter(n >= 100)


test1 <- word_tokens_int_hashed |> 
  filter(id <= 1000) |> 
  semi_join(temp1) |> 
  left_join(temp1) |> 
  select(id, word_ind)


# Test
test_pmi_4 <- test1 |> 
  filter(id <= 1000) |> 
  group_by(id) |> 
  mutate(word_ind2 = lead(word_ind),
         word_ind3 = lead(word_ind, 2L),
         word_ind4 = lead(word_ind, 3L),
         window_id = row_number()) |> 
  pivot_longer(word_ind:word_ind4, values_to = "word_ind") |> 
  select(-name) |> 
  slice_head(n = -12) |> 
  nest(words = c(word_ind, window_id))




tidy_pmi_prep <- word_tokens_int_hashed |> 
  group_by(id) |> 
  nest(words = word_ind) |> 
  ungroup() 

num_splits <- 50
for (i in seq_along(1:num_splits)) {
  # Specify the start and end rows for each of the sets.
  start_row <- round(nrow(tidy_pmi_prep)/num_splits * (i - 1)) + 1
  end_row <- round(nrow(tidy_pmi_prep)/num_splits * i)
  
  start <- tidy_pmi_prep$id[start_row]
  end <- tidy_pmi_prep$id[end_row]
  # Name assignment is based on index variable.
  pmi_name <- str_c("test_pmi_", i, ".rds")
  
  tidy_pmi <- word_tokens_int_hashed |> 
    filter(id >= start & id <= end) |> 
    group_by(id) |> 
    mutate(word_ind2 = lead(word_ind),
           word_ind3 = lead(word_ind, 2L),
           word_ind4 = lead(word_ind, 3L),
           word_ye = word_ind,
           window_id = row_number()) |> 
    pivot_longer(word_ind:word_ind4, values_to = "word_ind") |> 
    select(-name) |> 
    slice_head(n = -12) |> 
    nest(words = c(word_ind, window_id)) |> 
    write_rds(here::here("Data", pmi_name))
           
          
  rm(tidy_pmi, pmi_name)
}

tidy_pmi <- NULL

for (i in 1:num_splits) {
  # Recreate saved file names.
  pmi_name <- str_c("test_pmi_", i, ".rds")
  
  # Import saved files.
  test_pmi <- read_rds(here::here("Data", pmi_name))
  # Bind sliced data to main data frame.
  tidy_pmi <- tidy_pmi |>
    bind_rows(test_pmi)
  

  # Delete sliced tokenized data.
  #unlink(here::here("Data", tokens_name))
}


first_pmi <- read_rds(here::here("Data", "test_pmi_1.rds")) |> unnest(words) |> 
  unite(window_id, id, window_id)  
for (i in 2:num_splits) {
  # Recreate saved file names.
  pmi_name <- str_c("test_pmi_", i, ".rds")
  
  # Import saved files.
  test_pmi <- read_rds(here::here("Data", pmi_name))
  # Bind sliced data to main data frame.
  test_pmi <- test_pmi |> unnest(words) |> 
    unite(window_id, id, window_id) |> 
    pairwise_pmi(word_ind, window_id) %>%
    rename(new_pmi = pmi)
  
  first_pmi <- first_pmi |> 
  full_join(test_pmi, by = c("item1", "item2")) |> 
  mutate(pmi = pmi + new_pmi) |> 
  select(item1, item2, pmi)
  
  write_rds(first_pmi, here::here("Data", "first_pmi.rds"))

}


# Combined Split and PMI
window_size <- 4

num_splits <- 50
for (i in seq_along(1:num_splits)) {
  # Specify the start and end rows for each of the sets.
  start_row <- round(nrow(tidy_pmi_prep)/num_splits * (i - 1)) + 1
  end_row <- round(nrow(tidy_pmi_prep)/num_splits * i)
  
  start <- tidy_pmi_prep$id[start_row]
  end <- tidy_pmi_prep$id[end_row]
  # Name assignment is based on index variable.
  pmi_name <- str_c("test_pmi_", i, ".rds")
  
  tidy_pmi <- word_tokens_int_hashed |> 
    filter(id >= start & id <= end) |> 
    group_by(id) |> 
    mutate(word_ind2 = lead(word_ind),
           word_ind3 = lead(word_ind, 2L),
           word_ind4 = lead(word_ind, 3L),
           window_id = row_number()) |> 
    pivot_longer(word_ind:word_ind4, values_to = "word_ind") |> 
    select(-name) |> 
    slice_head(n = -12) |> 
    unite(window_id, id, window_id) |> 
    nest(words = c(word_ind, window_id)) |> 
    write_rds(here::here("Data", pmi_name))
  
  
  rm(tidy_pmi, pmi_name)
}



rm(word_tokens_int_hashed)

write_rds(tidy_pmi, here::here("Data", "tidy_pmi_4.rds"))
#### SELECT BEFORE PMI??
  

test_pmi <- test_pmi_50|> unnest(words) |> 
  unite(window_id, id, window_id) |> 
  pairwise_pmi(word_ind, window_id) |> 
  mutate(pmi_new = exp(pmi))

%>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

# tidy_pmi |> 
#   slice(1) |> 
#   unnest(words) |> 
#   select(id, year, quarter, word, window_id) |> 
#   unite(window_id, id, window_id) |> 
#   pairwise_pmi(word, window_id)
# 

# Creating the vectors
tidy_word_vectors <- test_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

# # Hurray!

# Save word_embeddings...

write_rds(tidy_word_vectors, here::here("Data", "tidy_word_vectors_4.rds"))

word_dict <- read_rds(here::here("Data", "word_dict"))

tidy_word_vectors <- tidy_word_vectors |> 
  rename(word_ind = item1) |> 
  mutate(word_ind = as.character(word_ind)) |> 
  left_join(word_dict, by = "word_ind") |> 
  select(word, dimension, value)

x <- list(1:81853)

pmi_tib <- tibble(
  item1 = seq(1:81853),
  occurences = 0,
  item2 = seq(1:81853),
  cooccurences = 0) |> 
  rowwise() |>
  mutate(item2 = list(seq(1:81853)))





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
    )(word, dimension, value) %>%
    select(-item2)
}

nearest_neighbors(tidy_word_vectors, "sales")



positive <- get_sentiments("loughran") |> 
  filter(sentiment == "positive") |> 
  select(word)
negative <- get_sentiments("loughran") |> 
  filter(sentiment == "negative") |> 
  select(word)




tidy_pmi <- read_rds(here::here("Data", "word_tokens_int_hashed.rds")) |> 
  group_by(id) |> 
  mutate(word_order = row_number()) 

word_dict <- read_rds(here::here("Data", "word_dict"))


lnm <- read_rds(here::here("Data", "lnm.rds"))
positive <- get_sentiments("loughran") |> 
  filter(sentiment == "positive") |> 
  select(word)
negative <- get_sentiments("loughran") |> 
  filter(sentiment == "negative") |> 
  select(word)


combo <- bind_rows(lnm, positive, negative)

num_splits <- 10
window_size <- 4

for (i in seq_along(1:num_splits)) {
  # Specify the start and end rows for each of the sets.
  start_row <- round(nrow(word_dict)/num_splits * (i - 1)) + 1
  end_row <- round(nrow(word_dict)/num_splits * i)
  
  # Name assignment is based on index variable.
  pmi_name <- str_c("test_pmi_new_", i, ".rds")
  tidy_pmi_loop <- tidy_pmi |> 
    filter(word_ind >= start_row & word_ind <= end_row) |> 
    mutate(word = word_ind)
  for (j in 1:window_size){
    
    nam <- str_c("word_ind", j)
  x <- tidy_pmi_loop |>
    select(id, word_order) |>
    mutate(word_order = word_order + j) |> 
    left_join(tidy_pmi) |> 
    select(word_ind) |>
    pull()
  
  
  tidy_pmi_loop[nam] <- x
  
  }
  
  tidy_pmi_loop  |>
    mutate(window_id = row_number()) |> 
    pivot_longer(contains("word_ind"), values_to = "word_ind") |> 
    select(-name) |> 
    slice_head(n = -(j*3)) |>
    ungroup() |> 
    group_by(word) |> 
    nest(words = c(word_ind, window_id)) |> 
    write_rds(here::here("Data", pmi_name))
  
  rm(tidy_pmi_loop, pmi_name)
}





combo <- combo |> left_join(word_dict) |> mutate(word_ind= as.integer(word_ind))

start_row <- round(nrow(word_dict)/num_splits * (i - 1)) + 1
end_row <- round(nrow(word_dict)/num_splits * i)

# Name assignment is based on index variable.


tidy_pmi_loop <- combo_code |> 
  left_join(tidy_pmi) |> select(-word)

for (j in 1:window_size){
  
  nam <- str_c("word_ind", j)
  x <- tidy_pmi_loop |>
    select(id, word_order) |>
    mutate(word_order = word_order + j) |> 
    left_join(tidy_pmi) |> 
    select(word_ind) |>
    pull()
  
  
  tidy_pmi_loop[nam] <- x
  
}

tidy_pmi_loop  |>
  mutate(window_id = row_number()) |> 
  pivot_longer(contains("word_ind"), values_to = "word_ind") |> 
  select(-name) |> 
  slice_head(n = -(j*3)) |>
  ungroup() |> 
  group_by(word) |> 
  nest(words = c(word_ind, window_id)) |> 
  write_rds(here::here("Data", pmi_name))

rm(tidy_pmi_loop, pmi_name)





tidy_pmi <- tidy_pmi |> 
  group_by(id) |> 
  mutate(word_ind2 = lead(word_ind),
         word_ind3 = lead(word_ind, 2L),
         word_ind4 = lead(word_ind, 3L),
         window_id = row_number())

tidy_pmi <- tidy_pmi |> semi_join(combo, by="word_ind")

pairwise_similarity()