# Visualize Word Counts ---------------------------------------------------
# Load packages.
library(tidyverse)
library(lubridate)
library(tidytext)
library(textdata)
library(corrr)
library(patchwork)

# Import word tokens and the marketing dictionary validated from 
# Loughran and McDonald's financial dictionary.
word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))
lnm <- read_rds(here::here("Data", "lnm.rds"))

# Indicate GICS subset.
ind_overa <- 0
ind_sectr <- 0
ind_group <- 1
# ind_indus <- 0
# ind_subin <- 0

# Specify name conditioned on indicator flags.
if (ind_overa == 1) name <- "overall"
if (ind_sectr == 1) name <- "sector"
if (ind_group == 1) name <- "group"
# if (ind_indus == 1) name <- "industry"
# if (ind_subin == 1) name <- "sub_industry"

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

# Visualize marketing terms.
if (ind_overa == 1) {
  word_counts |> 
    semi_join(lnm, by = "word") |> 
    arrange(desc(n)) |> 
    slice(1:n_words) |> 
    mutate(word = fct_reorder(word, n)) |> 
    ggplot(aes(x = n, y = word)) +
    geom_col() +
    labs(title = "Top Overall Marketing Terms")
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
    labs(title = str_c("Top Marketing Terms by ", str_to_title(name)))
}

ggsave(
  filename = here::here("Figures", str_c(name, "-marketing_terms.png")),
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

# Compute counts by id of marketing terms.
id_counts <- word_tokens |> 
  # Make year_quarter variable for easier time series plots.
  unite(year_quarter, year, quarter, sep=":", remove = FALSE) |> 
  # Compute counts and format variables.
  mutate(
    n_id = map_dbl(words, ~.x |> nrow()),                                      # Overall count.
    n_mktg = map_dbl(words, ~.x |> semi_join(lnm, by = "word") |> nrow()),     # Count of marketing terms.
    n_pos = map_dbl(words, ~.x |> semi_join(positive, by = "word") |> nrow()), # Count of positive terms.
    n_neg = map_dbl(words, ~.x |> semi_join(negative, by = "word") |> nrow()), # Count of negative terms.
    prop_mktg = n_mktg / n_id,                                                 # Proportion of marketing terms.
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

# Indicate GICS subset.
ind_overa <- 0
ind_sectr <- 0
ind_group <- 1
# ind_indus <- 0
# ind_subin <- 0

# Specify name conditioned on indicator flags.
if (ind_overa == 1) name <- "overall"
if (ind_sectr == 1) name <- "sector"
if (ind_group == 1) name <- "group"
# if (ind_indus == 1) name <- "industry"
# if (ind_subin == 1) name <- "sub_industry"

# Visualize the correlation matrix.
if (ind_overa == 1) {
  id_counts |> 
    select(revenue, earnings, difference, n_mktg:prop_neg) |> 
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
  group_names <- unique(id_counts[[name]])
  plot_list <- vector(mode = "list", length = length(group_names))
  for (i in seq_along(1:length(group_names))) {
     plot_list[[i]] <- id_counts |> 
      filter(.data[[name]] == group_names[i]) |>
      select(revenue, earnings, difference, n_mktg:prop_neg) |>
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
        title = str_c(
          "Correlation Matrix by ", 
          str_to_title(group_names[i]), " ", 
          str_to_title(name)
        ),
        x = "", y = ""
      )
  }
  if (ind_sectr == 1) {
    ( plot_list[[1]] | plot_list[[2]] | plot_list[[3]] ) / 
    ( plot_list[[4]] | plot_list[[5]] | plot_list[[6]] ) / 
    ( plot_list[[7]] | plot_list[[8]] | plot_list[[9]] ) / 
    ( plot_list[[10]] | plot_list[[11]] | grid::textGrob(" ") )
  }
  if (ind_group == 1) {
    ( plot_list[[1]] | plot_list[[2]] | plot_list[[3]] ) / 
    ( plot_list[[4]] | plot_list[[5]] | plot_list[[6]] ) / 
    ( plot_list[[7]] | plot_list[[8]] | plot_list[[9]] ) / 
    ( plot_list[[10]] | plot_list[[11]] | plot_list[[12]] ) /
    ( plot_list[[13]] | plot_list[[14]] | plot_list[[15]] ) /
    ( plot_list[[16]] | plot_list[[17]] | plot_list[[18]] ) /
    ( plot_list[[19]] | plot_list[[20]] | plot_list[[21]] )
  }
}

ggsave(
  filename = here::here("Figures", str_c(name, "-correlation.png")),
  width = 20, height = ifelse(ind_group == 0, 20, 35), units = "in", limitsize = FALSE
)

# # Marketing word proportion.
# clmd_tokens %>% filter(revenue!=0) %>%
#   ggplot(aes(x=year_quarter, y=word_prop)) +
#   geom_point(size=0.5)+
#   geom_quantile(size=0.5) +
#   geom_smooth(method=lm) +
#   labs(title = "Proportion of Marketing Words to Overall Words")
# 
# ggsave(
#   filename = here::here("Figures", "word_proportions_clmd.png"),
#   width = 4, height = 7, units = "in"
# )
# 
# # Revenue; both should be identical
# lnm_tokens %>% filter(revenue!=0) %>% 
#   ggplot(aes(x=year_quarter, y=revenue)) +
#   geom_point(size=0.5)+
#   geom_quantile(size=0.5) +
#   geom_smooth(method=lm) +
#   labs(title = "Revenue Over Time by Year/Quarter")
# 
# ggsave(
#   filename = here::here("Figures", "revenue_by_year_quarter.png"),
#   width = 4, height = 7, units = "in"
# )

