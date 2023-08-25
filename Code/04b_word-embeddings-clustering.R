library(tidyverse)
library(tidymodels)

# Load in pretrained word embeddings
w2v_vectors = read_csv(here::here("w2v.csv")) %>% select(-1) %>% drop_na()
glove_vectors = read_csv(here::here("glove.csv")) %>% select(-1) %>% drop_na()

# Simple K means, w2v
fit_tune_w2v <- tibble(k = 1:15) |>
  mutate(
    fit_km = pmap(list(centers = k), kmeans, x = w2v_vectors[1:300]),
    model_fit = map(fit_km, glance)
  ) |>
  unnest(model_fit) 

ggplot(fit_tune_w2v, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line()

# GloVe K means
fit_tune_glove <- tibble(k = 3:50) |>
  mutate(
    fit_km = pmap(list(centers = k), kmeans, x = glove_vectors[1:300]),
    model_fit = map(fit_km, glance)
  ) |>
  unnest(model_fit) 

ggplot(fit_tune_glove, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line()

# What I spent most of my time on was figuring out training our own embeddings.
# If it gets to that point, using the wordsalad package seems to be the best bet,
# however, it may be best to just go straight to big boy tranformers for that
# instead of GloVe or word2vec

library(wordsalad)
