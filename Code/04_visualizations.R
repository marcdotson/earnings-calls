# Mostly unsupervised visualizations.

library(tidyverse)
library(lubridate)

# Import marketing term lists, resave as .rds files

clmd <- read_csv(here::here("Data", "clmd.csv")) %>% mutate(word= str_to_lower(word))
lnm <- read_csv(here::here("Data", "marketing_words.csv")) %>% 
  mutate(word= str_to_lower(word))


write_rds(clmd, here::here("Data", "clmd.rds"))
write_rds(lnm, here::here("Data", "lnm.rds"))


clmd <- read_rds(here::here("Data", "clmd.rds"))
lnm <- read_rds(here::here("Data", "lnm.rds"))


# Import call data and call tokens
call_data <- read_rds(here::here("Data", "call_data.rds")) %>% 
  mutate(rowID = row_number()) %>% 
  select(-text)
call_tokens <- read_csv(here::here("Data", "call_tokens.csv"))


call_data <- call_data %>% mutate(rowID = row_number()) %>% select(-text)

call_tokens <- call_tokens %>% left_join(call_data, by="rowID")

lnm_tokens <- call_tokens %>%
  semi_join(lnm, by="word") %>%
  group_by(rowID) %>%
  count() %>% 
  rename(marketing_words=n) %>% 
  left_join(call_data, by="rowID") %>% rename(overall_words=n)

clmd_tokens <- call_tokens %>%
  semi_join(clmd, by="word") %>%
  group_by(rowID) %>%
  count() %>% 
  rename(marketing_words=n) %>% 
  left_join(call_data, by="rowID") %>% rename(overall_words=n)

write_rds(lnm_tokens, here::here("Data", "lnm_tokens.rds"))
write_rds(clmd_tokens, here::here("Data", "clmd_tokens.rds"))



# Visualizations

# Marketing word proportion--CLMD
clmd_tokens %>% filter(revenue!=0) %>%  ungroup() %>%
  unite(year_quarter, year, quarter, sep=":") %>% 
  mutate(year_quarter = yq(year_quarter),
         revenue = as.double(revenue),
         word_prop = marketing_words/overall_words) %>% 
  ggplot(aes(x=year_quarter, y=word_prop)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm)

# Revenue; both should be identical
clmd_tokens %>% filter(revenue!=0) %>%  ungroup() %>%
  unite(year_quarter, year, quarter, sep=":") %>% 
  mutate(year_quarter = yq(year_quarter),
         revenue = as.double(revenue),
         word_prop = marketing_words/overall_words) %>% 
  ggplot(aes(x=year_quarter, y=revenue)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm)



# Marketing word proportion--L&M

lnm_tokens %>% filter(revenue!=0) %>%  ungroup() %>%
  unite(year_quarter, year, quarter, sep=":") %>% 
  mutate(year_quarter = yq(year_quarter),
         revenue = as.double(revenue),
         word_prop = marketing_words/overall_words) %>% 
  ggplot(aes(x=year_quarter, y=word_prop)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm)



# Revenue; both should be identical
lnm_tokens %>% filter(revenue!=0) %>%  ungroup() %>%
  unite(year_quarter, year, quarter, sep=":") %>% 
  mutate(year_quarter = yq(year_quarter),
         revenue = as.double(revenue),
         word_prop = marketing_words/overall_words) %>% 
  ggplot(aes(x=year_quarter, y=revenue)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm)



# Word count, no stop words

  


# Overall word count by year.

# Word embeddings visualizations or as part of modeling?



