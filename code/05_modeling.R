# Clustering on Word Embeddings -------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)
library(topicmodels)

# Import completed word embeddings.
# Update placeholder code.

# K-means.
fit_tune_w2v <- tibble(k = 1:15) |>
  mutate(
    fit_km = pmap(list(centers = k), kmeans, x = w2v_vectors[1:300]),
    model_fit = map(fit_km, glance)
  ) |>
  unnest(model_fit) 

ggplot(fit_tune_w2v, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line()

# Affinity Propagation on Word Embeddings ---------------------------------



# Topic Modeling with Word Counts -----------------------------------------
# Import tokens and produce counts.
# Update placeholder code.

# Create a DTM.
dtm <- word_tokens %>%
  unnest(cols = words) |> 
  count(word, id) %>%
  cast_dtm(id, word, n)

# Tune k.
fit_tune <- tibble(num_topics = 2:20) %>%
  mutate(
    fit_lda = pmap(
      list(k = num_topics), 
      LDA, 
      x = dtm, method = "Gibbs"
    ),
    model_fit = map(fit_lda, logLik) %>% as.numeric()
  )

ggplot(fit_tune, aes(x = num_topics, y = model_fit)) +
  geom_point() + 
  geom_line()

# Fit a topic model.
set.seed(42)
fit_lda2 <- dtm_reviews %>% 
  LDA(k = 2, method = "Gibbs")

# Visualize.
fit_lda2 %>% 
  tidy(matrix = "beta") %>%
  group_by(topic) %>% 
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

