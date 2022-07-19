# Visualize Word Counts ---------------------------------------------------
# Load packages.
library(tidyverse)
library(lubridate)
library(tidytext)
library(textdata)
library(corrr)

# Import word tokens and marketing dictionaries.
word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))
clmd <- read_rds(here::here("Data", "clmd.rds")) # Common Language Marketing Dictionary.
lnm <- read_rds(here::here("Data", "lnm.rds"))   # Marketing terms from Loughran and McDonald.

# Indicate GICS subset.
ind_overa <- 1
ind_sectr <- 0
ind_group <- 0
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

# Visualize Aggregate Counts ----------------------------------------------
# Remove data that no longer needs to be held in memory.
rm(word_counts)

# Import and transform the L&M sentiment dictionary.
positive <- get_sentiments("loughran") |> 
  filter(sentiment == "positive") |> 
  select(word)
negative <- get_sentiments("loughran") |> 
  filter(sentiment == "negative") |> 
  select(word)

# Compute counts by id of marketing terms using lnm and clmd.
id_counts <- word_tokens |> 
  # Make year_quarter variable for easier time series plots.
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) |> 
  # Compute counts and format variables.
  mutate(
    n_id = map_dbl(words, ~.x |> nrow()),                                      # Overall count.
    n_lnm = map_dbl(words, ~.x |> semi_join(lnm, by = "word") |> nrow()),      # Count of lnm terms.
    n_clmd = map_dbl(words, ~.x |> semi_join(clmd, by = "word") |> nrow()),    # Count of clmd terms.
    n_pos = map_dbl(words, ~.x |> semi_join(positive, by = "word") |> nrow()), # Count of positive terms.
    n_neg = map_dbl(words, ~.x |> semi_join(negative, by = "word") |> nrow()), # Count of negative terms.
    prop_lnm = n_lnm / n_id,                                                   # Proportion of lnm terms.
    prop_clmd = n_clmd / n_id,                                                 # Proportion of clmd terms.
    prop_pos = n_pos / n_id,                                                   # Proportion of positive terms.
    prop_neg = n_neg / n_id,                                                   # Proportion of negative terms.
    year_quarter = yq(year_quarter)                                            # Format year_quarter.
  ) |> 
  # Remove words list-column to save memory.
  select(-words)

id_counts

# # Save and load intermediate steps, as needed.
# # write_rds(id_counts, here::here("Data", "id_counts.rds"))
# id_counts <- read_rds(here::here("Data", "id_counts.rds"))


# FACET CORRELATION MATRICES...


# Indicate GICS subset.
ind_overa <- 0
ind_sectr <- 1
ind_group <- 0
ind_indus <- 0
ind_subin <- 0

# Specify name conditioned on indicator flags.
if (ind_overa == 1) name <- "overall"
if (ind_sectr == 1) name <- "sector"
if (ind_group == 1) name <- "group"
if (ind_indus == 1) name <- "industry"
if (ind_subin == 1) name <- "sub_industry"

# Visualize the correlation matrix.
if (ind_overa == 1) {
  id_counts |> 
    select(revenue, earnings, forecast, difference, n_lnm:prop_neg) |> 
    correlate() |> 
    stretch() |>
    ggplot(aes(x = x, y = y, fill = r)) +
    geom_tile() +
    geom_text(aes(label = round(r, 2))) +
    scale_fill_gradient2(
      low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
      limits = c(-1, 1)
    ) +
    scale_x_discrete(expand=c(0.001,0.001)) +
    scale_y_discrete(expand=c(0.001,0.001)) +
    labs(
      title = "Correlation Matrix",
      subtitle = "Overall Correlation",
      x = "", y = ""
    )
}
if (ind_overa != 1) {
  id_counts |> 
    group_by(.data[[name]]) |> 
    select(revenue, earnings, forecast, difference, n_lnm:prop_neg) |> 
    ungroup() |> 
    correlate() |> 
    stretch() |>
    ggplot(aes(x = x, y = y, fill = r)) +
    geom_tile() +
    geom_text(aes(label = round(r, 2))) +
    facet_wrap(
      ~ .data[[name]], scales = "free",
      nrow = round(length(unique(id_counts[[name]])) / 3),
      ncol = round(length(unique(id_counts[[name]])) / 4)
    ) +
    scale_fill_gradient2(
      low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
      limits = c(-1, 1)
    ) +
    scale_x_discrete(expand=c(0.001,0.001)) +
    scale_y_discrete(expand=c(0.001,0.001)) +
    labs(
      title = "Correlation Matrix",
      subtitle = str_c("Correlation by ", str_to_title(name)),
      x = "", y = ""
    )
}

ggsave(
  filename = here::here("Figures", str_c(name, "-word_counts.png")),
  width = 10, height = 12, units = "in", limitsize = FALSE
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

