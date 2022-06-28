# Visualize Word Counts ---------------------------------------------------
# Load packages.
library(tidyverse)
library(lubridate)

# Import and transform data.
word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))  # Nested word tokens.
unnest_word_tokens <- word_tokens |> unnest(cols = words)       # Unnest word tokens.
overall_word_counts <- unnest_word_tokens |> count(word)        # Compute overall word tokens.
sector_word_counts <- unnest_word_tokens |> count(word, sector) # Compute word tokens by sector.
clmd <- read_rds(here::here("Data", "clmd.rds"))                # Common Language Marketing Dictionary terms.
lnm <- read_rds(here::here("Data", "lnm.rds"))                  # Loughran and McDonald dictionary terms.

overall_word_counts
sector_word_counts

# Save and load intermediate steps, as needed.
# write_rds(overall_word_counts, here::here("Data", "overall_word_counts.rds"))
# write_rds(sector_word_counts, here::here("Data", "sector_word_counts.rds"))
overall_word_counts <- read_rds(here::here("Data", "overall_word_counts.rds"))
sector_word_counts <- read_rds(here::here("Data", "sector_word_counts.rds"))

# Top 10 overall terms.
overall_word_counts |> 
  arrange(desc(n)) |> 
  slice(1:5) |> 
  mutate(word = fct_reorder(word, n)) |> 
  ggplot(aes(x = n, y = word)) +
  geom_col()

ggsave(
  filename = here::here("Figures", "overall-terms_top-10.png"),
  width = 4, height = 7, units = "in"
)

# Top 25 overall terms.
overall_word_counts |> 
  arrange(desc(n)) |> 
  slice(1:25) |> 
  mutate(word = fct_reorder(word, n)) |> 
  ggplot(aes(x = n, y = word)) +
  geom_col()

ggsave(
  filename = here::here("Figures", "overall-terms_top-25.png"),
  width = 4, height = 7, units = "in"
)

# Top 10 marketing terms using clmd.
overall_word_counts
  semi_join(clmd, by = "word") |> 
  arrange(desc(n)) |> 
  slice(1:10) |> 
  mutate(word = fct_reorder(word, n)) |> 
  ggplot(aes(x = n, y = word)) +
  geom_col()

ggsave(
  filename = here::here("Figures", "marketing-terms_top-10_clmd.png"),
  width = 4, height = 7, units = "in"
)

# Top 25 marketing terms using clmd.
overall_word_counts |> 
  semi_join(clmd, by = "word") |> 
  arrange(desc(n)) |> 
  slice(1:25) |> 
  mutate(word = fct_reorder(word, n)) |> 
  ggplot(aes(x = n, y = word)) +
  geom_col()

ggsave(
  filename = here::here("Figures", "marketing-terms_top-25_clmd.png"),
  width = 4, height = 7, units = "in"
)

# Top 10 marketing terms using lnm.
unnest_word_tokens |> 
  semi_join(lnm, by = "word") |> 
  count(word) |> 
  arrange(desc(n)) |> 
  slice(1:10) |> 
  mutate(word = fct_reorder(word, n)) |> 
  ggplot(aes(x = n, y = word)) +
  geom_col()

ggsave(
  filename = here::here("Figures", "marketing-terms_top-10_lnm.png"),
  width = 4, height = 7, units = "in"
)

# Top 25 marketing terms using lnm.
unnest_word_tokens |> 
  semi_join(lnm, by = "word") |> 
  count(word) |> 
  arrange(desc(n)) |> 
  slice(1:25) |> 
  mutate(word = fct_reorder(word, n)) |> 
  ggplot(aes(x = n, y = word)) +
  geom_col()

ggsave(
  filename = here::here("Figures", "marketing-terms_top-25_lnm.png"),
  width = 4, height = 7, units = "in"
)






fit_lda6 %>% 
  tidy(matrix = "beta") %>%
  group_by(topic) %>% 
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Remove data that no longer needs to be held in memory.
rm(unnest_word_tokens, overall_word_counts, sector_word_counts)

# Visualize Revenue Over Time ---------------------------------------------
# Count marketing words based on lnm terms.
lnm_tokens <- word_tokens |> 
  # Making year_quarter variable for easier time series.
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) |> 
  mutate(
    # Counting rows within the nested data.
    overall_words = map_dbl(words, ~.x |> nrow()),
    # Counting rows within the nested data after joining the dictionary.
    marketing_words = map_dbl(words, ~.x |> semi_join(lnm, by = "word") |> nrow()),
    # Formatting year_quarter.
    year_quarter = yq(year_quarter), 
    # Converting revenue to double.
    revenue = as.double(revenue),
    # Word proportions.
    word_prop = marketing_words / overall_words
  )

lnm_tokens

# Count marketing words based on clmd terms.
clmd_tokens <- word_tokens |> 
  # Making year_quarter variable for easier time series.
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) |> 
  mutate(
    # Counting rows within the nested data.
    overall_words = map_dbl(words, ~.x |> nrow()),
    # Counting rows within the nested data after joining the dictionary.
    marketing_words = map_dbl(words, ~.x |> semi_join(clmd, by = "word") |> nrow()),
    # Formatting year_quarter.
    year_quarter = yq(year_quarter), 
    # Converting revenue to double.
    revenue = as.double(revenue),
    # Word proportions.
    word_prop = marketing_words / overall_words
  )

clmd_tokens

# # Save and load intermediate steps, as needed.
# # write_rds(lnm_tokens, here::here("Data", "lnm_tokens.rds"))
# # write_rds(clmd_tokens, here::here("Data", "clmd_tokens.rds"))
# clmd_tokens <- read_rds(here::here("Data", "clmd_tokens.rds"))
# lnm_tokens <- read_rds(here::here("Data", "lnm_tokens.rds"))

# L&M marketing word proportions over time.
lnm_tokens |> 
  filter(revenue != 0) |>  
  ggplot(aes(x=year_quarter, y=word_prop)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm) +
  ggtitle()

# Save.

# Revenue; both should be identical
lnm_tokens %>% filter(revenue!=0) %>% 
  ggplot(aes(x=year_quarter, y=revenue)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm)

# Save.

# Marketing word proportion--CLMD
clmd_tokens %>% filter(revenue!=0) %>%
  ggplot(aes(x=year_quarter, y=word_prop)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm)

# Revenue; both should be identical
clmd_tokens %>% filter(revenue!=0) %>%  
  ggplot(aes(x=year_quarter, y=revenue)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm)





# Top words by year

x <- lnm_tokens |> ungroup() |> rowwise() |> 
  select(words, id) |>
  mutate(words = list(words |> semi_join(lnm, by="word") |> group_by(word) |> count()))
  


x |> unnest(words) |>
  left_join(call_data, by="id") |> 
  group_by(year) |> 
  summarise(n = sum(n)) |> 
  slice(1:20) %>% 
  ungroup %>% 
  mutate(word = reorder_within(word, n, year)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  facet_wrap(~ year, scales="free") +
  scale_y_reordered()



test12 <- lnm |> 
  summarise(n = map_df())


  filter(stopwords==input$stopwords,
         year >= input$from_year,
         year <= input$to_year) %>% 
  group_by(year) %>% 
  slice(1:input$n_words) %>% 
  ungroup %>% 
  mutate(word = reorder_within(word, n, year)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  facet_wrap(~ year, scales="free") +
  scale_y_reordered()


# Overall word count by year.




