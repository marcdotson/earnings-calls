# Mostly unsupervised visualizations.

library(tidyverse)
library(lubridate)
library(edgar) # Includes the LMMasterDictionary.

# Import marketing term lists, resave as .rds files

clmd <- read_csv(here::here("Data", "clmd.csv")) %>% mutate(word= str_to_lower(word))
lnm <- read_csv(here::here("Data", "marketing_words.csv")) %>% 
  mutate(word= str_to_lower(word))


write_rds(clmd, here::here("Data", "clmd.rds"))
write_rds(lnm, here::here("Data", "lnm.rds"))


clmd <- read_rds(here::here("Data", "clmd.rds"))
lnm <- read_rds(here::here("Data", "lnm.rds"))


# Import call data and word tokens

call_data <- read_rds(here::here("Data", "call_data.rds"))

word_tokens <- read_rds(here::here("Data", "word_tokens.rds")) |> ungroup()


lnm_tokens <- word_tokens %>%
  # Making year_quarter variable for easier time series
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) %>% 
  mutate(
    # Counting rows within the nested data
    overall_words = map_dbl(words, ~.x |> nrow()),
    # Counting rows within the nested data after joining dictionary
    marketing_words = map_dbl(words, ~.x |> semi_join(lnm, by = "word") |> nrow()),
    # Formatting year_quarter
    year_quarter = yq(year_quarter), 
    # Converting revenue to double
    revenue = as.double(revenue),
    # Word proportions
    word_prop = marketing_words/overall_words)

clmd_tokens <- word_tokens %>%
  # Making year_quarter variable for easier time series
  unite(year_quarter, year, quarter, sep=":",  remove = FALSE) %>% 
  mutate(
    # Counting rows within the nested data
    overall_words = map_dbl(words, ~.x |> nrow()),
    # Counting rows within the nested data after joining dictionary
    marketing_words = map_dbl(words, ~.x |> semi_join(clmd, by = "word") |> nrow()),
    # Formatting year_quarter
    year_quarter = yq(year_quarter), 
    # Converting revenue to double
    revenue = as.double(revenue),
    # Word proportions
    word_prop = marketing_words/overall_words)

# Writing and reloading token counts
write_rds(lnm_tokens, here::here("Data", "lnm_tokens.rds"))
write_rds(clmd_tokens, here::here("Data", "clmd_tokens.rds"))

clmd_tokens <- read_rds(here::here("Data", "clmd_tokens.rds"))
lnm_tokens <- read_rds(here::here("Data", "lnm_tokens.rds"))
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



# Marketing word proportion--L&M

lnm_tokens %>% filter(revenue!=0) %>% 
  ggplot(aes(x=year_quarter, y=word_prop)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm)



# Revenue; both should be identical
lnm_tokens %>% filter(revenue!=0) %>% 
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

# Word embeddings visualizations or as part of modeling?



