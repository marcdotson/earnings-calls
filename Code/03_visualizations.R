# Mostly unsupervised visualizations.

# Overall word count.


# Word count, no stop words
word_tot %>% 
  select(word, totals) %>%
  ungroup %>% 
  distinct() %>% 
  arrange(desc(totals)) %>%
  slice(1:30) %>% 
  mutate(word = fct_reorder(word, totals)) %>%
  ggplot(aes(x = totals, y = word)) +
  geom_col() 
  


# Overall word count by year.

lm_stopwords %>% 
  group_by(year) %>% 
  slice(1:10) %>% 
  ungroup %>% 
  mutate(word = reorder_within(word, n, year)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  facet_wrap(~ year, scales="free") +
  scale_y_reordered()


# test for flexdashboard 
word_counts %>% 
  filter(stopwords=="lm") %>% 
  group_by(year) %>% 
  slice(1:10) %>% 
  ungroup %>% 
  mutate(word = reorder_within(word, n, year)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  facet_wrap(~ year, scales="free") +
  scale_y_reordered()

# Word embeddings visualizations or as part of modeling?



# More code from SMLTA

tidy_word_vectors %>%
  filter(dimension <= 24) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup() %>%
  mutate(item1 = reorder_within(item1, value, dimension)) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Value",
    title = "Apple Earnings Calls",
    subtitle = paste("Top words contributing to the components that explain",
                     "the most variation")
  )