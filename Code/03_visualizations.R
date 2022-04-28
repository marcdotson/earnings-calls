# Mostly unsupervised visualizations.

# Overall word count.




# Overall word count by year.

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