# Visualize Word Counts ---------------------------------------------------
# Load packages.
library(tidyverse)
library(lubridate)
library(tidytext)

# Import word tokens and marketing dictionaries.
word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))
clmd <- read_rds(here::here("Data", "clmd.rds")) # Common Language Marketing Dictionary.
lnm <- read_rds(here::here("Data", "lnm.rds"))   # Marketing terms from Loughran and McDonald.

# Indicate GICS subset.
ind_overa <- 0
ind_sectr <- 0
ind_group <- 1
ind_indus <- 0
ind_subin <- 0

# Specify name conditioned on indicator flags.
if (ind_overa == 1) name <- "overall"
if (ind_sectr == 1) name <- "sector"
if (ind_group == 1) name <- "group"
if (ind_indus == 1) name <- "industry"
if (ind_subin == 1) name <- "sub_industry"

# Compute word counts.
if (ind_overa == 1) {
  word_counts <- word_tokens |> 
    unnest(cols = words) |> 
    count(word)
}
if (ind_overa != 1) {
  word_counts <- word_tokens |> 
    unnest(cols = words) |> 
    count(word, .data[[name]])
}

word_counts

# Visualize word counts.
n_words <- 10
if (ind_overa == 1) {
  word_counts |> 
    arrange(desc(n)) |> 
    slice(1:n_words) |> 
    mutate(word = fct_reorder(word, n)) |> 
    ggplot(aes(x = n, y = word)) +
    geom_col() +
    labs(title = "Top Overall Word Counts")
}
if (ind_overa != 1) {
  word_counts |> 
    group_by(.data[[name]]) |> 
    top_n(n_words, n) |> 
    ungroup() |> 
    mutate(word = reorder_within(word, n, .data[[name]])) |> 
    ggplot(aes(x = n, y = word, fill = as.factor(.data[[name]]))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(
      ~ .data[[name]], scales = "free",
      nrow = round(length(unique(word_counts[[name]])) / 3),
      ncol = round(length(unique(word_counts[[name]])) / 4)
    ) +
    scale_y_reordered() +
    labs(title = str_c("Top Word Counts by ", str_to_title(name)))
}

ggsave(
  filename = here::here("Figures", str_c(name, "-word_counts.png")),
  width = 10, height = 12, units = "in", limitsize = FALSE
)

# Visualize marketing terms using clmd.
if (ind_overa == 1) {
  word_counts |> 
    semi_join(clmd, by = "word") |> 
    arrange(desc(n)) |> 
    slice(1:n_words) |> 
    mutate(word = fct_reorder(word, n)) |> 
    ggplot(aes(x = n, y = word)) +
    geom_col() +
    labs(title = "Top Overall Marketing Terms (CLMD)")
}
if (ind_overa != 1) {
  word_counts |> 
    semi_join(clmd, by = "word") |> 
    group_by(.data[[name]]) |> 
    top_n(n_words, n) |> 
    ungroup() |> 
    mutate(word = reorder_within(word, n, .data[[name]])) |> 
    ggplot(aes(x = n, y = word, fill = as.factor(.data[[name]]))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(
      ~ .data[[name]], scales = "free",
      nrow = round(length(unique(word_counts[[name]])) / 3),
      ncol = round(length(unique(word_counts[[name]])) / 4)
    ) +
    scale_y_reordered() +
    labs(title = str_c("Top Marketing Terms (CLMD) by ", str_to_title(name)))
}

ggsave(
  filename = here::here("Figures", str_c(name, "-marketing_terms_clmd.png")),
  width = 10, height = 12, units = "in", limitsize = FALSE
)

# Visualize marketing terms using lnm.
if (ind_overa == 1) {
  word_counts |> 
    semi_join(lnm, by = "word") |> 
    arrange(desc(n)) |> 
    slice(1:n_words) |> 
    mutate(word = fct_reorder(word, n)) |> 
    ggplot(aes(x = n, y = word)) +
    geom_col() +
    labs(title = "Top Overall Marketing Terms (L&M)")
}
if (ind_overa != 1) {
  word_counts |> 
    semi_join(lnm, by = "word") |> 
    group_by(.data[[name]]) |> 
    top_n(n_words, n) |> 
    ungroup() |> 
    mutate(word = reorder_within(word, n, .data[[name]])) |> 
    ggplot(aes(x = n, y = word, fill = as.factor(.data[[name]]))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(
      ~ .data[[name]], scales = "free",
      nrow = round(length(unique(word_counts[[name]])) / 3),
      ncol = round(length(unique(word_counts[[name]])) / 4)
    ) +
    scale_y_reordered() +
    labs(title = str_c("Top Marketing Terms (L&M) by ", str_to_title(name)))
}

ggsave(
  filename = here::here("Figures", str_c(name, "-marketing_terms_lnm.png")),
  width = 10, height = 12, units = "in", limitsize = FALSE
)

# Correlation -------------------------------------------------------------
dash_tokens <- 
  # TIME SERIES PREP
  word_tokens |> 
  # Making year_quarter variable for easier time series.
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) |> 
  mutate(
    # Counting rows within the nested data.
    overall_count = map_dbl(words, ~.x |> nrow()),
    # Counting rows within the nested data after joining the dictionary.
    lnm_count = map_dbl(words, ~.x |> semi_join(lnm, by = "word") |> nrow()),
    clmd_count = map_dbl(words, ~.x |> semi_join(clmd, by = "word") |> nrow()),
    # Formatting year_quarter.
    year_quarter = yq(year_quarter), 
    # Converting revenue to double.
    revenue = as.double(revenue),
    # Word proportions.
    lnm_prop =  lnm_count/overall_count,
    clmd_count = clmd_count/overall_count
  ) |> 
  # NESTED MARKETING TERMS
  rowwise() |> 
  mutate(
    # New nested tibbles containing counts of each word by dictionary
    lnm_words = map(words, ~lnm |> rowwise() |>  mutate(x= (sum(.x %in% word)))),
    clmd_words = map(words, ~clmd |> rowwise() |>  mutate(x= (sum(.x %in% word))))) |>
  ungroup() |>
  # BAD IDEA
  mutate(sector = replace_na(sector, "N_A"),
         group = replace_na(group, "N_A"),
         industry = replace_na(industry, "N_A"),
         sub_industry = replace_na(sub_industry, "N_A"))

write_rds(dash_tokens, here::here("Data", "dash_tokens.rds"))

# Import sentiment dictionaries, courtesy of tidytext.
dash_tokens <- read_rds(here::here("Data", "dash_tokens.rds"))
loughran <- get_sentiments("loughran") # Loughran and McDonald sentiment dictionary.

# The following code matches word to their respective sentiments via the 
# dictionaries and generates counts of each sentiment.

# Start with overall word count, grouped by id.
loughran_count <- overall_word_counts |> 
  # Filter out words without sentiment value.
  semi_join(loughran) |>
  # Join sentiments to words. Words with multiple sentiments should be covered.
  left_join(loughran) |> 
  # Grouping by document id and sentiment.
  group_by(id, sentiment) |> 
  # Summing up the word counts in one column.
  summarise(sen_sum = sum(n)) |> 
  # Widening the data so each sentiment has its own column, each document is only 1 row.
  pivot_wider(names_from = sentiment, values_from = sen_sum) |> 
  # Ungrouping
  ungroup() |> 
  # Replacing all missing counts with 0.
  mutate(across(.cols = everything(), ~replace_na(.x, 0))) |> 
  # Renaming all sentiment columns to distinguish variable counts between dictionaries.
  rename_with(~ str_c(.x, "_loughran"), .cols = -id)

# Joining 
dash_counts <- dash_tokens |> select(-c("data", "words")) |> 
  left_join(loughran_count)

# Adding strings to the end of each row to differentiate duplicate organization
# levels e.g. Media group and Media industry
dash_counts <- dash_counts |> mutate(
  sector = str_c(sector, "sector", sep = " "),
  group = str_c(group, "group", sep = " "),
  industry = str_c(industry, "industry", sep = " "),
  sub_industry = str_c(sub_industry, "subindustry", sep = " ")
)

# Sector
org <- NULL
plot_name <- NULL
q <- NULL
for (i in 1:length(unique(dash_counts$sector))){
  
  org <- unique(dash_counts$sector)
  
  table_name <- str_c(org[i], "correlation_table.pdf", sep = "_")
  
  q <- dash_counts |> filter(
    sector == org[i]) |> 
    select(revenue, overall_count:trust_nrc) |>
    summarize(revenue = revenue,
              overall_count = overall_count,
              lnm_count = lnm_count,
              clmd_count = clmd_count,
              lnm_prop = round(lnm_prop, digits = 4),
              clmd_prop = round(clmd_prop, digits = 4),
              bing_pos_neg_prop = round(positive_bing/negative_bing, 4),
              bing_valence_prop = round(sum(across(contains("_bing")))/overall_count, 4),
              afinn_pos_neg_prop = round(sum(`1_afinn`, `2_afinn`, `3_afinn`,`4_afinn`,`5_afinn`)/sum(`-1_afinn`, `-2_afinn`, `-3_afinn`,`-4_afinn`,`-5_afinn`), 4),
              afinn_valence_prop = round(sum(across(contains("_afinn")))/overall_count, 4),
              loughran_pos_neg_prop = round(positive_loughran/negative_loughran, 4),
              loughran_valence_prop = round(sum(across(contains("_loughran")))/overall_count, 4),
              nrc_pos_neg_prop = round(positive_nrc/negative_nrc, 4),
              nrc_valence_prop = round(sum(across(contains("_nrc")))/overall_count, 4),
    ) |> cor()
  
  pdf(here::here("Figures", table_name))      # Export PDF
  grid.table(q)
  dev.off()
}







































# Visualize Revenue Over Time ---------------------------------------------
# Remove data that no longer needs to be held in memory.
rm(call_data, generic_stopwords)

# Count marketing words based on lnm terms.
lnm_tokens <- word_tokens |> 
  # Make a year_quarter variable for easier time series.
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) |> 
  mutate(
    # Count rows within the nested data.
    overall_words = map_dbl(words, ~.x |> nrow()),
    # Count rows within the nested data after joining the dictionary.
    marketing_words = map_dbl(words, ~.x |> semi_join(lnm, by = "word") |> nrow()),
    # Format year_quarter.
    year_quarter = yq(year_quarter), 
    # Convert revenue to double.
    revenue = as.double(revenue),
    # Compute word proportions.
    word_prop = marketing_words / overall_words
  )

lnm_tokens

# Count marketing words based on clmd terms.
clmd_tokens <- word_tokens |> 
  # Make a year_quarter variable for easier time series.
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) |> 
  mutate(
    # Count rows within the nested data.
    overall_words = map_dbl(words, ~.x |> nrow()),
    # Count rows within the nested data after joining the dictionary.
    marketing_words = map_dbl(words, ~.x |> semi_join(clmd, by = "word") |> nrow()),
    # Format year_quarter.
    year_quarter = yq(year_quarter), 
    # Convert revenue to double.
    revenue = as.double(revenue),
    # Compute word proportions.
    word_prop = marketing_words / overall_words
  )

clmd_tokens

# # Save and load intermediate steps, as needed.
# # write_rds(lnm_tokens, here::here("Data", "lnm_tokens.rds"))
# # write_rds(clmd_tokens, here::here("Data", "clmd_tokens.rds"))
# clmd_tokens <- read_rds(here::here("Data", "clmd_tokens.rds"))
# lnm_tokens <- read_rds(here::here("Data", "lnm_tokens.rds"))

dash_tokens <- 
  # TIME SERIES PREP
  word_tokens |> 
  # Making year_quarter variable for easier time series.
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) |> 
  mutate(
    # Counting rows within the nested data.
    overall_count = map_dbl(words, ~.x |> nrow()),
    # Counting rows within the nested data after joining the dictionary.
    lnm_count = map_dbl(words, ~.x |> semi_join(lnm, by = "word") |> nrow()),
    clmd_count = map_dbl(words, ~.x |> semi_join(clmd, by = "word") |> nrow()),
    # Formatting year_quarter.
    year_quarter = yq(year_quarter), 
    # Converting revenue to double.
    revenue = as.double(revenue),
    # Word proportions.
    lnm_prop =  lnm_count/overall_count,
    clmd_count = clmd_count/overall_count
  ) |> 
  # NESTED MARKETING TERMS
  rowwise() |> 
  mutate(
    # New nested tibbles containing counts of each word by dictionary
    lnm_words = map(words, ~lnm |> rowwise() |>  mutate(x= (sum(.x %in% word)))),
    clmd_words = map(words, ~clmd |> rowwise() |>  mutate(x= (sum(.x %in% word))))) |>
  ungroup() |>
  # BAD IDEA
  mutate(sector = replace_na(sector, "N_A"),
         group = replace_na(group, "N_A"),
         industry = replace_na(industry, "N_A"),
         sub_industry = replace_na(sub_industry, "N_A"))

write_rds(dash_tokens, here::here("Data", "dash_tokens.rds"))







# Graph loops

dash_counts <- dash_counts |>  mutate(sector = str_replace_all(sector,"N/A", "N_A"),
                                      group = str_replace_all(group, "N/A","N_A"),
                                      industry = str_replace_all(industry,"N/A", "N_A"),
                                      sub_industry = str_replace_all(sub_industry,"N/A", "N_A")) |> 
  mutate(sector = str_replace_all(sector," ", "_"),
         group = str_replace_all(group, " ","_"),
         industry = str_replace_all(industry," ", "_"),
         sub_industry = str_replace_all(sub_industry," ", "_"))


# Sector
org <- NULL
plot_name <- NULL
for (i in 1:length(unique(dash_counts$sector))){
  
  org <- unique(dash_counts$sector)
  
  plot_name <- str_c(org[i], "average_words.png", sep = "_")
  
  dash_counts |> filter(
    sector == org[i]) |> 
    group_by(year_quarter) |> 
    summarize(mean_overall_count = mean(overall_count),
              mean_lnm_count = mean(lnm_count),
              mean_clmd_count = mean(clmd_count)) |> 
    ggplot(aes(x = year_quarter)) +
    geom_col(aes(y = mean_overall_count), fill = "gray", alpha = 0.25, position = "stack") +
    geom_col(aes(y = mean_lnm_count), fill = "green", alpha = 0.5,  position = "stack") +
    geom_col(aes(y = mean_clmd_count), fill = "blue", alpha = 0.5,  position = "stack") +
    labs(title = plot_name, x = "Year_Quarter", y = "Average word count")
  
  ggsave(
    filename = here::here("Figures", plot_name),
    width = 10, height = 6, units = "in"
  )
}

# L&M marketing word proportions over time.
lnm_tokens |> 
  filter(revenue != 0) |>  
  ggplot(aes(x=year_quarter, y=word_prop)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm) +
  labs(title = "Proportion of Marketing Words to Overall Words (L&M)")

ggsave(
  filename = here::here("Figures", "word_proportions_lnm.png"),
  width = 4, height = 7, units = "in"
)


# Marketing word proportion--CLMD
clmd_tokens %>% filter(revenue!=0) %>%
  ggplot(aes(x=year_quarter, y=word_prop)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm) +
  labs(title = "Proportion of Marketing Words to Overall Words (CLMD)")

ggsave(
  filename = here::here("Figures", "word_proportions_clmd.png"),
  width = 4, height = 7, units = "in"
)

# Revenue; both should be identical
lnm_tokens %>% filter(revenue!=0) %>% 
  ggplot(aes(x=year_quarter, y=revenue)) +
  geom_point(size=0.5)+
  geom_quantile(size=0.5) +
  geom_smooth(method=lm) +
  labs(title = "Revenue Over Time by Year/Quarter")

ggsave(
  filename = here::here("Figures", "revenue_by_year_quarter.png"),
  width = 4, height = 7, units = "in"
)














# Sector
org <- NULL
plot_name <- NULL
for (i in 1:length(unique(dash_counts$sector))){
  
  org <- unique(dash_counts$sector)
  
  plot_name <- str_c(org[i], "average_revenue.png", sep = "_")
  
  dash_counts |> filter(
    sector == org[i]) |> 
    group_by(year_quarter) |> 
    summarize(mean_revenue = mean(revenue)) |> 
    ggplot(aes(x = year_quarter)) +
    geom_col(aes(y = mean_revenue), fill = "gray", alpha = 0.9) +
    labs(title = plot_name, x = "Year_Quarter", y = "Average Revenue")
  
  ggsave(
    filename = here::here("Figures", plot_name),
    width = 10, height = 6, units = "in"
  )
}




# Sentiment ---------------------------------------------------------------
# Import sentiment dictionaries, courtesy of tidytext.
dash_tokens <- read_rds(here::here("Data", "dash_tokens.rds"))
loughran <- get_sentiments("loughran") # Loughran and McDonald sentiment dictionary.

# The following code matches word to their respective sentiments via the 
# dictionaries and generates counts of each sentiment.

# Start with overall word count, grouped by id.
loughran_count <- overall_word_counts |> 
  # Filter out words without sentiment value.
  semi_join(loughran) |>
  # Join sentiments to words. Words with multiple sentiments should be covered.
  left_join(loughran) |> 
  # Grouping by document id and sentiment.
  group_by(id, sentiment) |> 
  # Summing up the word counts in one column.
  summarise(sen_sum = sum(n)) |> 
  # Widening the data so each sentiment has its own column, each document is only 1 row.
  pivot_wider(names_from = sentiment, values_from = sen_sum) |> 
  # Ungrouping
  ungroup() |> 
  # Replacing all missing counts with 0.
  mutate(across(.cols = everything(), ~replace_na(.x, 0))) |> 
  # Renaming all sentiment columns to distinguish variable counts between dictionaries.
  rename_with(~ str_c(.x, "_loughran"), .cols = -id)

# Joining 
dash_counts <- dash_tokens |> select(-c("data", "words")) |> 
  left_join(loughran_count)

# Adding strings to the end of each row to differentiate duplicate organization
# levels e.g. Media group and Media industry
dash_counts <- dash_counts |> mutate(
  sector = str_c(sector, "sector", sep = " "),
  group = str_c(group, "group", sep = " "),
  industry = str_c(industry, "industry", sep = " "),
  sub_industry = str_c(sub_industry, "subindustry", sep = " ")
)

# write_rds(dash_counts, here::here("Earnings_Call_Shiny", "dash_counts.rds"))






# Sector
org <- NULL
plot_name <- NULL
for (i in 1:length(unique(dash_counts$sector))){
  
  org <- unique(dash_counts$sector)
  
  plot_name <- str_c(org[i], "afinn_sentiment.png", sep = "_")

  dash_counts |> filter(
    sector == org[i]) |>
    group_by(year_quarter) |>
    summarize(across(.cols = contains("afinn"), ~mean(.x))) |>
    pivot_longer(names_to = "valence", values_to = "n") |>
    ggplot(aes(x = year_quarter, fill = valence)) +
    geom_col(aes(y = n)) +
    labs(title = plot_name, x = "Year_Quarter", y = "Average word count")

  ggsave(
    filename = here::here("Figures", plot_name),
    width = 10, height = 6, units = "in")
}

