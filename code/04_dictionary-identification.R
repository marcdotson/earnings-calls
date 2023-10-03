# Word Tokens and Pre-Trained Word Embeddings -----------------------------
# Load packages.
library(tidyverse)
library(tidymodels)
library(tidytext)
library(topicmodels)

# Import word tokens.
# word_tokens <- read_rds(here::here("data", "word_tokens.rds")) |>
#   select(id, words) |>
#   unnest(cols = words)
# 
# word_tokens
# 
# # set.seed(42)
# # word_tokens |>
# #   sample_n(size = 1000000) |> 
# #   write_delim(here::here("data", "word_tokens_sample.txt"))

word_tokens <- read_delim(here::here("data", "word_tokens_sample.txt"))

word_tokens

# De-duplicate word tokens to form a dictionary. Unlike topic
# modeling, co-occurrence doesn't inform clustering algorithms.
word_dictionary <- word_tokens |> 
  distinct()

word_dictionary

# Specify number of embeddings/dimensions and import pre-trained word embeddings 
# (data downloaded from https://nlp.stanford.edu/projects/glove/).
n_embeddings <- 50
glove_embeddings <- read_delim(
  here::here("data", "glove", str_c("glove.6B.", n_embeddings, "d.txt")),
  delim = " ",
  quote = "", # Default quote = "\"" was stopping import at the term """.
  col_names = c("word", str_c("emb", 1:n_embeddings))
)

glove_embeddings

# Join the dictionary and embeddings.
word_embeddings <- word_dictionary |> 
  inner_join(glove_embeddings, by = "word") |> 
  select(-id)

word_embeddings

# Topic Modeling with Word Counts -----------------------------------------
# Create a document-term matrix.
dtm <- word_tokens |>
  count(word, id) |>
  cast_dtm(id, word, n)

# Tune k.
set.seed(42)
lda_tune <- tibble(k = 2:101, fit = NA)
for (k in 1:length(lda_tune$k)) {
  lda_fit <- LDA(x = dtm, k = lda_tune$k[k], method = "Gibbs")
  lda_tune$fit[k] <- logLik(lda_fit) |> as.numeric()
}

ggplot(lda_tune, aes(x = k, y = fit)) +
  geom_point() +
  geom_line() +
  labs(title = "LDA Elbow Plot")

ggsave(here::here("figures", "clustering-lda_tune.png"), width = 8, height = 5, units = "in")

# Summarize the best-fitting topic model.
set.seed(42)
lda_fit <- LDA(x = dtm, k = 25, method = "Gibbs")

lda_fit |>
  tidy(matrix = "beta") |>
  group_by(topic) |>
  top_n(10, beta) |>
  ungroup() |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(x = beta, y = term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Clustering on Pre-Trained Word Embeddings -------------------------------
# Select the clustering variables.
km_data <- word_embeddings |> select(-word)

# Tune k.
set.seed(42)
km_tune <- tibble(k = 1:100, fit = NA)
for (k in 1:length(km_tune$k)) {
  km_fit <- kmeans(km_data, centers = k, iter.max = 100)
  km_tune$fit[k] <- glance(km_fit)$tot.withinss
}

ggplot(km_tune, aes(x = k, y = fit)) +
  geom_point() +
  geom_line() +
  labs(title = "K-Means Elbow Plot")

ggsave(here::here("figures", "clustering-km_tune.png"), width = 8, height = 5, units = "in")

# Summarize the best-fitting clustering algorithm.
set.seed(42)
km_fit <- kmeans(km_data, centers = 25, iter.max = 100)
km_data <- augment(km_fit, word_embeddings)

km_data |>
  # group_by(.cluster) |>
  # top_n(10, beta) |>
  # ungroup() |>
  # mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(x = beta, y = term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Affinity Propagation on Pre-Trained Word Embeddings ---------------------

