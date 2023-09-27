# Word Tokens and Pre-Trained Word Embeddings -----------------------------
# Load packages.
library(tidyverse)
library(tidytext)
library(topicmodels)

# Import word tokens.
word_tokens <- read_rds(here::here("data", "word_tokens.rds")) |> 
  select(id, words) |> 
  unnest(cols = words)

word_tokens

# write_delim(word_tokens, here::here("data", "word_tokens.txt"))
# word_tokens <- read_delim(here::here("data", "word_tokens.txt"))

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
  quote = "", # Default quote = "\"" was stopping import at the term ".
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
dtm <- word_tokens %>%
  count(word, id) %>%
  cast_dtm(id, word, n)



# Fit a topic model.
set.seed(42)
fit_lda2 <- dtm |> 
  LDA(k = 2, method = "Gibbs")

# Will we run into the same long vector error?


# Tune k.
tune_lda <- tibble(num_topics = 2:20) %>%
  mutate(
    fit_lda = pmap(
      list(k = num_topics),
      LDA,
      x = dtm, method = "Gibbs"
    ),
    model_fit = map(fit_lda, logLik) %>% as.numeric()
  )

# FOR LOOP?

ggplot(tune_lda, aes(x = num_topics, y = model_fit)) +
  geom_point() +
  geom_line()

# Visualize the best-fitting topic model.
set.seed(42)
fit_lda <- dtm %>%
  LDA(k = 2, method = "Gibbs")

fit_lda %>%
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Clustering on Pre-Trained Word Embeddings -------------------------------

# Fit a k-means model.
set.seed(42)
fit_km2 <- word_embeddings |> 
  select(-word) |> 
  sample_n(size = 100000) |> 
  # as.data.frame() |> 
  kmeans(centers = 2)

# Error in do_one(nmeth) : long vectors (argument 1) are not supported in .Fortran
# OR R crashes...1000 rows works, 10000 rows works, 100000 rows ___

# Tune k.
tune_km <- tibble(k = 1:10) |>
  mutate(
    fit_km = pmap(list(centers = k), kmeans, x = word_embeddings[,2:(n_embeddings + 1)]),
    augment_data = map(fit_km, augment, data = word_embeddings[,2:(n_embeddings + 1)])
  ) |> 
  unnest(augment_data)

# # Select the clustering variables.
# sim_obs <- sim_data |> 
#   select(x, y)
# 
# # Tune k.
# set.seed(42)
# fit_tune <- tibble(k = 1:10) |>
#   mutate(
#     fit_km = pmap(list(centers = k), kmeans, x = sim_obs),
#     model_fit = map(fit_km, glance)
#   ) |> 
#   unnest(model_fit)

ggplot(tune_km, aes(x = x, y = y)) +
  geom_point(aes(color = .cluster), alpha = 0.5) + 
  facet_wrap(~ k)

# Summarize the best-fitting clustering algorithm.


# Affinity Propagation on Pre-Trained Word Embeddings ---------------------

