# Using the word embeddings.

# Topic modeling.
# Load packages.
library(tidyverse)
library(tidytext)
library(topicmodels)

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

# Supervised learning using the revenue.
