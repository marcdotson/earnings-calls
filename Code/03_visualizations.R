# Mostly unsupervised visualizations.

# Overall word count.

# Word counts after filtering with different stop word lexicons

# No stop words
no_stopwords <- word_tokens %>%
  group_by(year, quarter) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "no stopwords")

# Loughran McDonald stop words
lm_stopwords <- word_tokens_lm %>% 
  group_by(year, quarter) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "lm")

# Tidytext stop words (SMART, Snowball, ISO)
tidytext_stopwords <- word_tokens_tt %>% 
  group_by(year, quarter) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "tidytext")

# SMART stop words
smart_stopwords <- word_tokens_smart %>% 
  group_by(year, quarter) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "smart")

# Snowball stop words
snowball_stopwords <- word_tokens_snowball %>% 
  group_by(year, quarter) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "snowball")

# ISO stop words
iso_stopwords <- word_tokens_iso %>% 
  group_by(year, quarter) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "iso")

# onix stop words
onix_stopwords <- word_tokens_onix %>% 
  group_by(year, quarter) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "onix")

# All stop word lexicons
all_stopwords <- word_tokens_all %>% 
  group_by(year, quarter) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(stopwords = "all")
 
# Binding previous data frames, grouped by year and quarter
word_counts <- no_stopwords %>% 
  bind_rows(tidytext_stopwords,
            lm_stopwords,
            smart_stopwords,
            iso_stopwords,
            snowball_stopwords,
            onix_stopwords,
            all_stopwords)

# Word totals, grouped by word/stop word
word_tot <- word_counts %>%
  ungroup() %>% 
  select(word, n, stopwords) %>% 
  group_by(word, stopwords) %>% 
  summarise(totals = sum(n))


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