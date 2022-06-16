# Mostly unsupervised visualizations.


# reimport for fun
call_data <- read_rds(here::here("Data", "call_data.rds")) %>%
  mutate(rowID = row_number()) %>% 
  select(year, rowID)

call_tokens <- call_tokens %>% 
  left_join(call_data, by = "rowID")

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


word_tokens %>% 
  inner_join(terms) %>% 
  count(word) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x = n, y = word)) +
  geom_col()

call_tokens <- read_csv(here::here("Data", "call_tokens.csv"))

call_tokens %>% 
  inner_join(terms) %>% 
  count(word) %>%
  arrange(desc(n)) %>% 
  slice(1:25) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = n, y = word)) +
  geom_col()





# top 10 marketing terms by year
call_tokens %>% 
  inner_join(terms) %>% 
  group_by(year) %>% 
  count(word) %>%
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  ungroup %>% 
  mutate(word = reorder_within(word, n, year)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  facet_wrap(~ year, scales="free") +
  scale_y_reordered()



# top 10 words by year
call_tokens %>% 
  group_by(year) %>% 
  count(word) %>%
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  ungroup %>% 
  mutate(word = reorder_within(word, n, year)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  facet_wrap(~ year, scales="free") +
  scale_y_reordered()

# 25 overall word count
call_tokens %>% 
  count(word) %>%
  arrange(desc(n)) %>% 
  slice(1:25) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = n, y = word)) +
  geom_col()


# Word embeddings visualizations or as part of modeling?



# More code from SMLTA

tidy_word_vectors %>%
  filter(dimension <= 24) %>%
  group_by(dimension) %>%
  top_n(8, abs(value)) %>%
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