# Visualize Word Counts ---------------------------------------------------
# Load packages.
library(tidyverse)
library(lubridate)
library(tidytext)
library(textdata)
library(corrr)
library(patchwork)
library(latex2exp)

# Import word tokens and the marketing dictionary validated from 
# Loughran and McDonald's financial dictionary.
word_tokens <- read_rds(here::here("data", "word_tokens.rds"))
lnm <- read_rds(here::here("data", "lnm.rds"))

# Indicate GICS subset.
ind_overa <- 1
ind_sectr <- 0
ind_group <- 0
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
    labs(title = "Top 10 Words", y = "")
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
    labs(title = str_c("Top 10 Words by ", str_to_title(name)))
}

ggsave(
  filename = here::here("figures", str_c(name, "-word_counts.png")),
  width = 7, height = 5, units = "in", limitsize = FALSE
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
    labs(title = "Top 10 Marketing Words", y = "")
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
    labs(title = str_c("Top 10 Marketing Words by ", str_to_title(name)))
}

ggsave(
  filename = here::here("figures", str_c(name, "-marketing_terms.png")),
  width = 7, height = 5, units = "in", limitsize = FALSE
)

# Visualize Aggregate Counts ----------------------------------------------
# Remove data that no longer needs to be held in memory.
rm(word_counts)

# Import the top advertisers from each calendar year.
top_ads <- read_csv(here::here("data", "Top Advertisers.csv")) |>
  mutate(
    name = str_to_upper(name),
    name = str_remove(name, "\\.$")
  )

# Import and transform the L&M sentiment dictionary.
positive <- get_sentiments("loughran") |> 
  filter(sentiment == "positive") |> 
  select(word)
negative <- get_sentiments("loughran") |> 
  filter(sentiment == "negative") |> 
  select(word)

# Import the future words dictionary.
future <- read_csv(here::here("data", "Future Focused Words.csv"))

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
    n_futr = map_dbl(words, ~.x |> semi_join(future, by = "word") |> nrow()),  # Count of future terms.
    prop_mktg = n_mktg / n_id,                                                 # Proportion of marketing terms.
    prop_pos = n_pos / n_id,                                                   # Proportion of positive terms.
    prop_neg = n_neg / n_id,                                                   # Proportion of negative terms.
    prop_futr = n_futr / n_id,                                                 # Proportion of future terms.
    year_quarter = yq(year_quarter)                                            # Format year_quarter.
  ) |> 
  # Lead prop_mktg data by firm.
  group_by(gvkey) |> 
  arrange(year_quarter) |> 
  mutate(prop_mktg_lead = lead(prop_mktg, order_by = year_quarter)) |> 
  ungroup() |>
  # Rename variables for plotting.
  rename(
    surprise = difference,
    positive = prop_pos,
    negative = prop_neg,
    marketing = prop_mktg,
    mktg_lead = prop_mktg_lead,
    future = prop_futr
  ) |> 
  # Remove words list-column to save memory.
  select(-words)

id_counts

# Save and load intermediate steps, as needed.
# write_rds(id_counts, here::here("Data", "id_counts.rds"))
id_counts <- read_rds(here::here("data", "id_counts.rds"))

# Visualize the overall correlation matrix.
# Run correlation tests.
cor_test <- id_counts |>
  select(
    # Consider outcome variables (appear in reverse order).
    earnings, surprise, revenue,
    # Considered explanatory variables (appear in reverse order).
    future, negative, positive, marketing
  ) |> 
  as.matrix() |>
  Hmisc::rcorr()

# Extract p-values and indicate significance level.
pvalues <- cor_test$P |> 
  as_cordf() |> 
  # shave(upper = TRUE) |>
  stretch() |> 
  rename(pvalue = r) |> 
  mutate(
    asterisk = case_when(
      pvalue > 0.05 ~ "",
      pvalue > 0.01 & pvalue <= 0.05 ~ "*",
      pvalue > 0.001 & pvalue <= 0.01 ~ "**",
      pvalue <= 0.001 ~ "***",
    )
  )

# Plot the correlation matrix.
id_counts |> 
  select(
    # Consider outcome variables (appear in reverse order).
    earnings, surprise, revenue,
    # Considered explanatory variables (appear in reverse order).
    future, negative, positive, marketing
  ) |> 
  correlate() |> 
  # shave(upper = TRUE) |>
  stretch() |>
  left_join(pvalues) |> 
  mutate(
    x = fct_inorder(x), 
    y = fct_inorder(y),
    r_asterisk = str_c(round(r, 2), asterisk)
  ) |>
  ggplot(aes(x = as.factor(x), y = as.factor(y), fill = r)) +
  geom_tile() +
  geom_text(aes(label = r_asterisk)) +
  scale_fill_gradient2(
    low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
    limits = c(-1, 1)
  ) +
  scale_x_discrete(expand=c(0.001,0.001), position = "top") +
  scale_y_discrete(expand=c(0.001,0.001)) +
  theme(plot.title = element_text(vjust = -2)) +
  labs(
    title = "Correlation Matrix",
    caption = TeX("* p-value $\\leq$ 0.05, ** p-value $\\leq$ 0.01, *** p-value $\\leq$ 0.001"),
    x = "", y = ""
  )

ggsave(
  filename = here::here("figures", "overall-correlation.png"),
  width = 7, height = 6, units = "in", limitsize = FALSE
)

# Visualize the Consumer Discretionary Sector correlation matrix.
# Run correlation tests.
cor_test <- id_counts |>
  filter(sector == "Consumer Discretionary") |> 
  select(
    # Consider outcome variables (appear in reverse order).
    earnings, surprise, revenue,
    # Considered explanatory variables (appear in reverse order).
    future, negative, positive, marketing
  ) |> 
  as.matrix() |>
  Hmisc::rcorr()

# Extract p-values and indicate significance level.
pvalues <- cor_test$P |> 
  as_cordf() |> 
  # shave(upper = TRUE) |>
  stretch() |> 
  rename(pvalue = r) |> 
  mutate(
    asterisk = case_when(
      pvalue > 0.05 ~ "",
      pvalue > 0.01 & pvalue <= 0.05 ~ "*",
      pvalue > 0.001 & pvalue <= 0.01 ~ "**",
      pvalue <= 0.001 ~ "***",
    )
  )

# Plot the correlation matrix.
id_counts |> 
  filter(sector == "Consumer Discretionary") |> 
  select(
    # Consider outcome variables (appear in reverse order).
    earnings, surprise, revenue,
    # Considered explanatory variables (appear in reverse order).
    future, negative, positive, marketing
  ) |> 
  correlate() |> 
  # shave(upper = TRUE) |>
  stretch() |>
  left_join(pvalues) |> 
  mutate(
    x = fct_inorder(x), 
    y = fct_inorder(y),
    r_asterisk = str_c(round(r, 2), asterisk)
  ) |>
  ggplot(aes(x = as.factor(x), y = as.factor(y), fill = r)) +
  geom_tile() +
  geom_text(aes(label = r_asterisk)) +
  scale_fill_gradient2(
    low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
    limits = c(-1, 1)
  ) +
  scale_x_discrete(expand=c(0.001,0.001), position = "top") +
  scale_y_discrete(expand=c(0.001,0.001)) +
  theme(plot.title = element_text(vjust = -2)) +
  labs(
    title = "Correlation Matrix for Consumer Discretionary Sector",
    caption = TeX("* p-value $\\leq$ 0.05, ** p-value $\\leq$ 0.01, *** p-value $\\leq$ 0.001"),
    x = "", y = ""
  )

ggsave(
  filename = here::here("figures", "sector-consumer_discretionary-correlation.png"),
  width = 7, height = 6, units = "in", limitsize = FALSE
)

# Visualize the Consumer Durables & Apparel correlation matrix.
# Run correlation tests.
cor_test <- id_counts |>
  filter(group == "Consumer Durables & Apparel") |> 
  select(
    # Consider outcome variables (appear in reverse order).
    earnings, surprise, revenue,
    # Considered explanatory variables (appear in reverse order).
    future, negative, positive, marketing
  ) |> 
  as.matrix() |>
  Hmisc::rcorr()

# Extract p-values and indicate significance level.
pvalues <- cor_test$P |> 
  as_cordf() |> 
  # shave(upper = TRUE) |>
  stretch() |> 
  rename(pvalue = r) |> 
  mutate(
    asterisk = case_when(
      pvalue > 0.05 ~ "",
      pvalue > 0.01 & pvalue <= 0.05 ~ "*",
      pvalue > 0.001 & pvalue <= 0.01 ~ "**",
      pvalue <= 0.001 ~ "***",
    )
  )

# Plot the correlation matrix.
id_counts |> 
  filter(group == "Consumer Durables & Apparel") |> 
  select(
    # Consider outcome variables (appear in reverse order).
    earnings, surprise, revenue,
    # Considered explanatory variables (appear in reverse order).
    future, negative, positive, marketing
  ) |> 
  correlate() |> 
  # shave(upper = TRUE) |>
  stretch() |>
  left_join(pvalues) |> 
  mutate(
    x = fct_inorder(x), 
    y = fct_inorder(y),
    r_asterisk = str_c(round(r, 2), asterisk)
  ) |>
  ggplot(aes(x = as.factor(x), y = as.factor(y), fill = r)) +
  geom_tile() +
  geom_text(aes(label = r_asterisk)) +
  scale_fill_gradient2(
    low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
    limits = c(-1, 1)
  ) +
  scale_x_discrete(expand=c(0.001,0.001), position = "top") +
  scale_y_discrete(expand=c(0.001,0.001)) +
  theme(plot.title = element_text(vjust = -2)) +
  labs(
    title = "Correlation Matrix for Consumer Durables & Apparel Group",
    caption = TeX("* p-value $\\leq$ 0.05, ** p-value $\\leq$ 0.01, *** p-value $\\leq$ 0.001"),
    x = "", y = ""
  )

ggsave(
  filename = here::here("figures", "group-consumer_durables-correlation.png"),
  width = 7, height = 6, units = "in", limitsize = FALSE
)

# Visualize the top advertisers' correlation matrix.
# Run correlation tests.
cor_test <- top_ads |> 
  inner_join(id_counts, by = c("name", "year")) |> 
  select(
    # Consider outcome variables (appear in reverse order).
    earnings, surprise, revenue,
    # Considered explanatory variables (appear in reverse order).
    future, negative, positive, marketing
  ) |> 
  as.matrix() |>
  Hmisc::rcorr()

# Extract p-values and indicate significance level.
pvalues <- cor_test$P |> 
  as_cordf() |> 
  # shave(upper = TRUE) |>
  stretch() |> 
  rename(pvalue = r) |> 
  mutate(
    asterisk = case_when(
      pvalue > 0.05 ~ "",
      pvalue > 0.01 & pvalue <= 0.05 ~ "*",
      pvalue > 0.001 & pvalue <= 0.01 ~ "**",
      pvalue <= 0.001 ~ "***",
    )
  )

# Plot the correlation matrix.
top_ads |> 
  inner_join(id_counts, by = c("name", "year")) |> 
  select(
    # Consider outcome variables (appear in reverse order).
    earnings, surprise, revenue,
    # Considered explanatory variables (appear in reverse order).
    future, negative, positive, marketing
  ) |> 
  correlate() |> 
  # shave(upper = TRUE) |>
  stretch() |>
  left_join(pvalues) |> 
  mutate(
    x = fct_inorder(x), 
    y = fct_inorder(y),
    r_asterisk = str_c(round(r, 2), asterisk)
  ) |>
  ggplot(aes(x = as.factor(x), y = as.factor(y), fill = r)) +
  geom_tile() +
  geom_text(aes(label = r_asterisk)) +
  scale_fill_gradient2(
    low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
    limits = c(-1, 1)
  ) +
  scale_x_discrete(expand=c(0.001,0.001), position = "top") +
  scale_y_discrete(expand=c(0.001,0.001)) +
  theme(plot.title = element_text(vjust = -2)) +
  labs(
    title = "Correlation Matrix for the US Top Advertisers from 2000-2020",
    caption = TeX("* p-value $\\leq$ 0.05, ** p-value $\\leq$ 0.01, *** p-value $\\leq$ 0.001"),
    x = "", y = ""
  )

ggsave(
  filename = here::here("figures", "top_advertisers-correlation.png"),
  width = 7, height = 6, units = "in", limitsize = FALSE
)  

# Indicate GICS subset.
# ind_overa <- 0
ind_sectr <- 0
ind_group <- 1
# ind_indus <- 0
# ind_subin <- 0

# Specify name conditioned on indicator flags.
# if (ind_overa == 1) name <- "overall"
if (ind_sectr == 1) name <- "sector"
if (ind_group == 1) name <- "group"
# if (ind_indus == 1) name <- "industry"
# if (ind_subin == 1) name <- "sub_industry"  

group_names <- unique(id_counts[[name]])[!map_lgl(unique(id_counts[[name]]), ~.x |> is.na())]
plot_list <- vector(mode = "list", length = length(group_names))
for (i in seq_along(1:length(group_names))) {
  # Run correlation tests.
  cor_test <- id_counts |>
    filter(.data[[name]] == group_names[i]) |>
    select(
      # Consider outcome variables (appear in reverse order).
      earnings, surprise, revenue,
      # Considered explanatory variables (appear in reverse order).
      future, negative, positive, marketing
    ) |> 
    as.matrix() |>
    Hmisc::rcorr()
  
  # Extract p-values and indicate significance level.
  pvalues <- cor_test$P |> 
    as_cordf() |> 
    # shave(upper = TRUE) |>
    stretch() |> 
    rename(pvalue = r) |> 
    mutate(
      asterisk = case_when(
        pvalue > 0.05 ~ "",
        pvalue > 0.01 & pvalue <= 0.05 ~ "*",
        pvalue > 0.001 & pvalue <= 0.01 ~ "**",
        pvalue <= 0.001 ~ "***",
      )
    )
  
  # Plot the correlation matrix.
  plot_list[[i]] <- id_counts |> 
    filter(.data[[name]] == group_names[i]) |>
    select(
      # Consider outcome variables (appear in reverse order).
      earnings, surprise, revenue,
      # Considered explanatory variables (appear in reverse order).
      future, negative, positive, marketing
    ) |> 
    correlate() |> 
    # shave(upper = TRUE) |>
    stretch() |>
    left_join(pvalues) |> 
    mutate(
      x = fct_inorder(x), 
      y = fct_inorder(y),
      r_asterisk = str_c(round(r, 2), asterisk)
    ) |>
    
    ggplot(aes(x = as.factor(x), y = as.factor(y), fill = r)) +
    geom_tile() +
    geom_text(aes(label = r_asterisk)) +
    scale_fill_gradient2(
      low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
      limits = c(-1, 1)
    ) +
    scale_x_discrete(expand=c(0.001,0.001), position = "top") +
    scale_y_discrete(expand=c(0.001,0.001)) +
    theme(plot.title = element_text(vjust = -2)) +
    # theme(plot.title = element_text(vjust = -2, size = 25)) +
    labs(
      title = str_c(str_to_title(group_names[i])),
      # title = str_c("Correlation Matrix for ", str_to_title(group_names[i]), " ", str_to_title(name)),
      caption = TeX("* p-value $\\leq$ 0.05, ** p-value $\\leq$ 0.01, *** p-value $\\leq$ 0.001"),
      x = "", y = ""
    )
}

if (ind_sectr == 1) {
  # plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + plot_list[[4]] + plot_list[[5]] + 
  #   plot_list[[6]] + plot_list[[7]] + plot_list[[8]] + plot_list[[9]] + plot_list[[10]] + 
  #   plot_list[[11]] + plot_spacer() + 
  #   plot_layout(
  #     ncol = 3, nrow = 4, byrow = TRUE,
  #     widths = 7, heights = 6
  #   )
  # 
  # ggsave(
  #   filename = here::here("Figures", str_c(name, "-correlation.png")),
  #   width = 21, height = 24, units = "in", limitsize = FALSE
  # )
  
  plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + plot_list[[4]] + plot_list[[5]] +
    plot_list[[6]] + 
    plot_layout(
      ncol = 2, nrow = 3, byrow = TRUE,
      widths = 7, heights = 6
    )

  ggsave(
    filename = here::here("figures", str_c(name, "-correlation-01.png")),
    width = 14, height = 18, units = "in", limitsize = FALSE
  )

  plot_list[[7]] + plot_list[[8]] + plot_list[[9]] + plot_list[[10]] + plot_list[[11]] + 
    plot_spacer() +
    plot_layout(
      ncol = 2, nrow = 3, byrow = TRUE,
      widths = 7, heights = 6
    )

  ggsave(
    filename = here::here("figures", str_c(name, "-correlation-02.png")),
    width = 14, height = 18, units = "in", limitsize = FALSE
  )
}
if (ind_group == 1) {
  # plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + plot_list[[4]] + plot_list[[5]] + 
  #   plot_list[[6]] + plot_list[[7]] + plot_list[[8]] + plot_list[[9]] + plot_list[[10]] + 
  #   plot_list[[11]] + plot_list[[12]] + plot_list[[13]] + plot_list[[14]] + plot_list[[15]] + 
  #   plot_list[[16]] + plot_list[[17]] + plot_list[[18]] + plot_list[[19]] + plot_list[[20]] + 
  #   plot_list[[21]] + plot_spacer() + plot_spacer() + plot_spacer() + 
  #   plot_layout(
  #     ncol = 4, nrow = 6, byrow = TRUE,
  #     widths = 7, heights = 6
  #   )
  # 
  # ggsave(
  #   filename = here::here("Figures", str_c(name, "-correlation.png")),
  #   width = 28, height = 36, units = "in", limitsize = FALSE
  # )
  
  plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + plot_list[[4]] + plot_list[[5]] + 
    plot_list[[6]] + 
    plot_layout(
      ncol = 2, nrow = 3, byrow = TRUE,
      widths = 7, heights = 6
    )

  ggsave(
    filename = here::here("figures", str_c(name, "-correlation-01.png")),
    width = 14, height = 18, units = "in", limitsize = FALSE
  )

  plot_list[[7]] + plot_list[[8]] + plot_list[[9]] + plot_list[[10]] + 
    plot_list[[11]] + plot_list[[12]] + 
    plot_layout(
      ncol = 2, nrow = 3, byrow = TRUE,
      widths = 7, heights = 6
    )

  ggsave(
    filename = here::here("figures", str_c(name, "-correlation-02.png")),
    width = 14, height = 18, units = "in", limitsize = FALSE
  )

  plot_list[[13]] + plot_list[[14]] + plot_list[[15]] + 
    plot_list[[16]] + plot_list[[17]] + plot_list[[18]] + 
    plot_layout(
      ncol = 2, nrow = 3, byrow = TRUE,
      widths = 7, heights = 6
    )

  ggsave(
    filename = here::here("figures", str_c(name, "-correlation-03.png")),
    width = 14, height = 18, units = "in", limitsize = FALSE
  )

  plot_list[[19]] + plot_list[[20]] + 
    plot_list[[21]] + plot_spacer() + 
    plot_layout(
      ncol = 2, nrow = 2, byrow = TRUE,
      widths = 7, heights = 6
    )

  ggsave(
    filename = here::here("figures", str_c(name, "-correlation-04.png")),
    width = 14, height = 12, units = "in", limitsize = FALSE
  )
}

# # Proportion of marketing terms over time.
# id_counts |> 
#   # filter(revenue!=0) |>
#   ggplot(aes(x = year_quarter, y = prop_mktg)) +
#   geom_point(size = 0.5)+
#   geom_quantile(size = 0.5) +
#   geom_smooth(method = lm) +
#   labs(title = "Proportion of Marketing Terms Over Time")
# 
# # ggsave(
# #   filename = here::here("Figures", "word_proportions_clmd.png"),
# #   width = 4, height = 7, units = "in"
# # )
# 
# # Revenue; both should be identical
# lnm_tokens %>% filter(revenue!=0) %>%
#   ggplot(aes(x=year_quarter, y=revenue)) +
#   geom_point(size=0.5)+
#   geom_quantile(size=0.5) +
#   geom_smooth(method=lm) +
#   labs(title = "Revenue Over Time by Year/Quarter")
# 
# # ggsave(
# #   filename = here::here("figures", "revenue_by_year_quarter.png"),
# #   width = 4, height = 7, units = "in"
# # )

