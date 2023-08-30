# Load packages
library(tidyverse)
library(tidytext)
library(widyr)
library(furrr)
library(text2vec)
library(igraph)
library(ggraph)
library(tidygraph)


plan(multisession, workers= 8)  ## for parallel processing


# Indicate GICS subset.
ind_overa <- 0
ind_sectr <- 1
ind_group <- 0
# ind_indus <- 0
# ind_subin <- 0

# Specify name conditioned on indicator flags.
if (ind_overa == 1) name <- "overall"
if (ind_sectr == 1) name <- "sector"
if (ind_group == 1) name <- "group"

# Load Sentiment
positive <- get_sentiments("loughran") |> 
  filter(sentiment == "positive") |> 
  select(word) 
negative <- get_sentiments("loughran") |> 
  filter(sentiment == "negative") |> 
  select(word)

# Load marketing terms
lnm <- read_rds(here::here("Data", "lnm.rds"))
lnm <- distinct(lnm)

# Load word tokens
word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))

# Recombine words for text2vec format
test1 <- word_tokens |>
  transmute(text= future_map_chr(words, ~paste0(.x, collapse = " ")),
                                  id = id)

top_ads <- read_csv(here::here("Data", "Top Advertisers.csv")) |>
  mutate(
    name = str_to_upper(name),
    name = str_remove(name, "\\.$")
  )


# Write
write_rds(test1, here::here("Data", "test1.rds"))

# Reload recombined tokens
test1 <- read_rds(here::here("Data", "test1.rds"))

# text2vec corpus creation--specify text, tokenizer, number of chunks, document
# id
ivec <- itoken(test1$text, tokenizer = word_tokenizer, n_chunks=8, ids=test1$id)

# text2vec vocabulary, used in later functions
voc <- create_vocabulary(ivec)

# text2vec vocabulary, creating window size of 4. 
voc_four <- create_vocabulary(ivec, window_size = 4L)

# Filtering vocabulary by words with low word count
voc_filtered <- voc |> filter(term_count >= 100)

# Vectorizer helps computations
vec <- vocab_vectorizer(voc_filtered)

# Create a term co-occurrence matrix
tcm_4 <- create_tcm(ivec, vec, skip_grams_window = 4L)

# Saving because it takes a minute to run
write_rds(tcm_4, here::here("Data", "coll_mat.rds"))

# First collocation, I believe it filters out word which co occur less than 50
# times by default, which is why I run it again.
m1 <- Collocations$new(vocabulary=voc_filtered, pmi_min = 0.0001)
m1$fit(ivec)

# The text2vec documentation is pretty decent
pq <- m1$collocation_stat 
write_rds(pq, here::here("Data", "coll_tib.rds"))
pq <- m1$collocation_stat |> select(prefix, suffix, pmi) |>
  rename(item1 = prefix, item2 = suffix)

# word embeddings using the widyr package
word_embeddings <- pq %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

word_embeddings
write_rds(word_embeddings, here::here("Data", "word_embeddings.rds"))


# nearest neighbors function is pulled from SMLTAR book, potentially another
# way of getting collocations, but I don't understand it well enough to use
# it.
nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) / 
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) %>%
    select(-item2)
}

nearest_neighbors(word_embeddings, "sales")


# Overall Collocations
m2 <- Collocations$new(vocabulary=voc_filtered, collocation_count_min = 0,
                       pmi_min = 0.0001)
m2$partial_fit(ivec)


pq2 <- m2$collocation_stat 

write_rds(pq2, here::here("Data", "coll_tib_all.rds"))
pq2 <- read_rds(here::here("Data", "coll_tib_all.rds"))



ivec <- itoken(test1$text, tokenizer = word_tokenizer, n_chunks=8, ids=test1$id)
voc_lnm <- create_vocabulary(combo$word, window_size = 4L)

vec_lnm <- vocab_vectorizer(voc_lnm)

test1 <- read_rds(here::here("Data", "test1.rds"))


mktg_mktg <- pq2 |>
  select(prefix, suffix, n_i, n_j, n_ij, pmi, llr) |>
  filter(prefix %in% lnm$word, suffix %in% lnm$word)
  # complete(prefix, suffix, fill = list(n_ij = 0)) |> 
  # filter(n_ij!=0) |> filter(prefix != suffix, llr >= 100)

top_mktg <- mktg_mktg |> select(prefix, n_i) |> distinct() |>
  arrange(desc(n_i)) |> slice(1:20)

mktg_tbl <- mktg_mktg |> select(prefix, suffix, llr) |>
  rename(from = prefix, to = suffix, weight = llr)

growth_tbl <- mktg_mktg |>
  select(prefix, suffix, llr) |>
  filter(prefix %in% top_mktg$prefix, suffix %in% top_mktg$prefix)  |>
  rename(from = prefix, to = suffix, weight =llr) |> 
  as_tbl_graph() |> simplify()

sent_tbl <- mktg_mktg |>
  select(prefix, suffix, llr) |>
  filter(prefix %in% top_mktg$prefix, suffix %in% loughran$to)  |>
  rename(from = prefix, to = suffix, weight =llr) |> 
  left_join(loughran) |> 
  mutate(weight= if_else(sentiment=="positive", weight, -1*weight)) |> 
  group_by(from) |> 
  summarise(sent_weight= sum(weight)) |> 
  mutate(valence= if_else(sent_weight >= 0, "positive", "negative"))


growth_tbl <- mktg_mktg |>
  select(prefix, suffix, llr) |>
  filter(prefix %in% top_mktg$prefix, suffix %in% top_mktg$prefix)  |>
  rename(from = prefix, to = suffix, weight =llr) |> 
  left_join(sent_tbl) |> 
  mutate(valence = NULL) |> 
  as_tbl_graph() |> simplify()


V(growth_tbl)$sent_weight <- sent_tbl$sent_weight
V(growth_tbl)$valence <- sent_tbl$valence

# growth_tbl <- create_layout(growth_tbl, layout="dendrogram",length=weight)

V(growth_tbl)$sent_weight <- sent_tbl$sent_weight
ggraph(growth_tbl) +
  geom_edge_link(aes(alpha = weight), color= "black" ) +
  scale_edge_alpha_continuous("weight") +
  geom_node_point(aes(color = valence, alpha = sent_weight), size = 7) +
  geom_node_label(aes(label = name), size= 4,
                  label.padding = .1, repel = TRUE) +
  labs(title = "Top 20 Marketing Terms Collocation Overall") +
  scale_color_manual(values = c("negative" = "red",
                                "positive"="blue")) +
  scale_alpha_continuous("sent_weight")

ggsave(
  filename = here::here("Figures", "Overall_Collocation.png"),
  width = 12, height = 12, units = "in", limitsize = FALSE
)


# Collocations by surprise
m2 <- Collocations$new(vocabulary=voc_filtered, collocation_count_min = 0,
                       pmi_min = 0.0001)
m2$partial_fit(ivec)


pq2 <- m2$collocation_stat 

write_rds(pq2, here::here("Data", "coll_tib_all.rds"))
pq2 <- read_rds(here::here("Data", "coll_tib_all.rds"))

test1 <- read_rds(here::here("Data", "test1.rds")) |> left_join(word_tokens, by = "id")

goodtib <- test1 |> filter(difference > 0)
badtib <- test1 |> filter(difference < 0 )



badvec <- itoken(badtib$text, tokenizer = word_tokenizer, n_chunks=8, ids=badtib$id)
goodvec <- itoken(goodtib$text, tokenizer = word_tokenizer, n_chunks=8, ids=goodtib$id)

vec_lnm <- vocab_vectorizer(voc_lnm)


bad_tib <- w
mktg_mktg <- pq2 |>
  select(prefix, suffix, n_i, n_j, n_ij, pmi, llr) |>
  filter(prefix %in% lnm$word, suffix %in% lnm$word)
# complete(prefix, suffix, fill = list(n_ij = 0)) |> 
# filter(n_ij!=0) |> filter(prefix != suffix, llr >= 100)

top_mktg <- mktg_mktg |> select(prefix, n_i) |> distinct() |>
  arrange(desc(n_i)) |> slice(1:20)

mktg_tbl <- mktg_mktg |> select(prefix, suffix, llr) |>
  rename(from = prefix, to = suffix, weight = llr)

growth_tbl <- mktg_mktg |>
  select(prefix, suffix, llr) |>
  filter(prefix %in% top_mktg$prefix, suffix %in% top_mktg$prefix)  |>
  rename(from = prefix, to = suffix, weight =llr) |> 
  as_tbl_graph() |> simplify()

sent_tbl <- mktg_mktg |>
  select(prefix, suffix, llr) |>
  filter(prefix %in% top_mktg$prefix, suffix %in% loughran$to)  |>
  rename(from = prefix, to = suffix, weight =llr) |> 
  left_join(loughran) |> 
  mutate(weight= if_else(sentiment=="positive", weight, -1*weight)) |> 
  group_by(from) |> 
  summarise(sent_weight= sum(weight)) |> 
  mutate(valence= if_else(sent_weight >= 0, "positive", "negative"))


growth_tbl <- mktg_mktg |>
  select(prefix, suffix, llr) |>
  filter(prefix %in% top_mktg$prefix, suffix %in% top_mktg$prefix)  |>
  rename(from = prefix, to = suffix, weight =llr) |> 
  left_join(sent_tbl) |> 
  mutate(valence = NULL) |> 
  as_tbl_graph() |> simplify()


V(growth_tbl)$sent_weight <- sent_tbl$sent_weight
V(growth_tbl)$valence <- sent_tbl$valence

# growth_tbl <- create_layout(growth_tbl, layout="dendrogram",length=weight)

V(growth_tbl)$sent_weight <- sent_tbl$sent_weight
ggraph(growth_tbl) +
  geom_edge_link(aes(alpha = weight), color= "black" ) +
  scale_edge_alpha_continuous("weight") +
  geom_node_point(aes(color = valence, alpha = sent_weight), size = 7) +
  geom_node_label(aes(label = name), size= 4,
                  label.padding = .1, repel = TRUE) +
  labs(title = "Top 20 Marketing Terms Collocation Overall") +
  scale_color_manual(values = c("negative" = "red",
                                "positive"="blue")) +
  scale_alpha_continuous("sent_weight")

ggsave(
  filename = here::here("Figures", "Overall_Collocation.png"),
  width = 12, height = 12, units = "in", limitsize = FALSE
)


# Top Advertiser Collocations
set.seed(123)

wt_grouped <- word_tokens |> select(id, name, year)

test1 <- read_rds(here::here("Data", "test1.rds")) |> left_join(wt_grouped)

temp1 <- test1 |> inner_join(top_ads, by = c("name", "year"))


ivec <- itoken(temp1$text, tokenizer = word_tokenizer, n_chunks=8, ids=temp1$id)
voc_four <- create_vocabulary(ivec, window_size = 4L)

vec <- vocab_vectorizer(voc_four)

m2 <- Collocations$new(vocabulary=voc_four, collocation_count_min = 5,
                       pmi_min = 0.0001)
m2$partial_fit(ivec)


pq2 <- m2$collocation_stat 


mktg_mktg <- pq2 |>
  select(prefix, suffix, n_i, n_j, n_ij, pmi, llr, lfmd) |>
  filter(prefix %in% lnm$word, suffix %in% lnm$word)

top_mktg <- mktg_mktg |> select(prefix, n_i) |> distinct() |>
  arrange(desc(n_i)) |> slice(1:20)


sent_tbl_llr <- mktg_mktg |>
  select(prefix, suffix, llr) |>
  filter(prefix %in% top_mktg$prefix, suffix %in% loughran$to)  |>
  rename(from = prefix, to = suffix, weight =llr) |> 
  left_join(loughran) |> 
  mutate(weight= if_else(sentiment=="positive", weight, -1*weight)) |> 
  group_by(from) |> 
  summarise(sent_weight= sum(weight)) |> 
  ungroup() |> 
  mutate(valence= if_else(sent_weight >= 0, "positive", "negative"))


mktg_tbl_llr <- mktg_mktg |>
  select(prefix, suffix, llr) |>
  filter(prefix %in% top_mktg$prefix, suffix %in% top_mktg$prefix)  |>
  rename(from = prefix, to = suffix, weight =llr) |> 
  left_join(sent_tbl_llr) |> 
  as_tbl_graph() |> simplify()


V(mktg_tbl_llr)$sent_weight <- sent_tbl_llr$sent_weight
V(mktg_tbl_llr)$valence <- sent_tbl_llr$valence


plot_list_llr <- ggraph(mktg_tbl_llr) +
  geom_edge_link(aes(alpha = weight), color= "black" ) +
  scale_edge_alpha_continuous("weight") +
  geom_node_point(aes(color = valence, alpha = sent_weight), size = 7) +
  geom_node_label(aes(label = name), size= 4, label.padding = .1,
                  repel = TRUE) +
  labs(title = str_c(
    "Top 20 Marketing Terms LLR Collocation for Top US Advertisers 2000-2020")) +
  scale_color_manual(values = c("negative" = "red",
                                "positive"="blue")) +
  scale_alpha_continuous("sent_weight")

ggsave(
  filename = here::here("Figures", "Top-Advertiser-LLR-collocation.png"),
  width = 12, height = 12, units = "in", limitsize = FALSE)


# Collocations

set.seed(123)
loughran_bin <- bind_rows(positive, negative)

loughran <- get_sentiments("loughran") |> semi_join(loughran_bin) |>
  slice(1:2477) |> rename(to=word)



tcm <- NULL
tib <- NULL
# Data prep for graphing
if (ind_overa != 1) {
  wt_grouped <- word_tokens |> select(id, .data[[name]])
  
  test1 <- read_rds(here::here("Data", "test1.rds")) |> left_join(wt_grouped)
  
  group_names <- unique(word_tokens[[name]])[!map_lgl(
    unique(word_tokens[[name]]), ~.x |> is.na())]

  for (i in seq_along(1:length(group_names))) {
    
    temp1 <- test1 |> filter(.data[[name]] == group_names[i])
    ivec <- itoken(temp1$text, tokenizer = word_tokenizer, n_chunks=8,
                   ids=temp1$id)
    voc_four <- create_vocabulary(ivec, window_size = 4L)
    
    vec <- vocab_vectorizer(voc_four)
    
    m2 <- Collocations$new(vocabulary=voc_four,
                           collocation_count_min = 5, pmi_min = 0.0001)
    m2$partial_fit(ivec)
    
    
    pq2 <- m2$collocation_stat 
    
    
    
    mktg_mktg <- pq2 |>
      select(prefix, suffix, n_i, n_j, n_ij, pmi, llr, lfmd) |>
      filter(prefix %in% lnm$word, suffix %in% lnm$word) |> 
      mutate(gics = group_names[i],
             jaccard = (n_ij)/(n_i + n_j - n_ij),
             cosine = (n_ij)/(sqrt(n_i*n_j)))
    
    top_mktg <- mktg_mktg |> select(prefix, n_i) |> distinct() |>
      arrange(desc(n_i)) |> slice(1:20)
    
    
    tib <- tib |> bind_rows(mktg_mktg)
    
    voc_four <- create_vocabulary(top_mktg$prefix, window_size = 4L)
    
    vec <- vocab_vectorizer(voc_four)
    
    tcm <- create_tcm(ivec, vec, skip_grams_window = 4L)
    
    write_rds(tcm, here::here("Data", str_c(group_names[i], "_tcm.rds")))
    x <- sim2(tcm, method = "cosine")
    
    coltib <- tibble(
      word_col= x@Dimnames[[1]],
      col_match = x@p[2:21])
    
    rowtib <- tibble(
      word_row = x@Dimnames[[1]],
      row_match = seq(0:19)-1)
    
    q <- tibble(
      row = x@i) |> 
      mutate(xd = as.double(x@x),
             row_match = row,
             col_match = ifelse(xd >= .99, row_number(), NA)) |> 
      left_join(coltib, by = "col_match") |> 
      fill(word_col, .direction = "up") |> 
      left_join(rowtib) 
    
    q_filter <- q |> select(word_col, word_row, xd) |>
      filter(word_col != word_row)
    write_rds(q_filter, here::here("Data", str_c(group_names[i],
                                                 "_cosine.rds")))
}

}
write_rds(tib, here::here("Data", str_c(name, "coll_tib.rds")))



if (ind_overa != 1) {
  wt_grouped <- word_tokens |> select(id, .data[[name]])
  
  test1 <- read_rds(here::here("Data", "test1.rds")) |> left_join(wt_grouped)
  
  group_names <- unique(word_tokens[[name]])[!map_lgl(
    unique(word_tokens[[name]]), ~.x |> is.na())]
  
  for (i in seq_along(1:length(group_names))) {
    
    tcm <- read_rds(here::here("Data", str_c(group_names[i], "_tcm.rds")))
    x <- sim2(tcm, method = "cosine")
    
    coltib <- tibble(
      word_col= x@Dimnames[[1]],
      col_match = x@p[2:21])
    
    rowtib <- tibble(
      word_row = x@Dimnames[[1]],
      row_match = seq(0:19)-1)
    
    q <- tibble(
      row = x@i) |> 
      mutate(xd = as.double(x@x),
             row_match = row,
             col_match = ifelse(xd >= .99, row_number(), NA)) |> 
      left_join(coltib, by = "col_match") |> 
      fill(word_col, .direction = "up") |> 
      left_join(rowtib) 
    
    q_filter <- q |> select(word_col, word_row, xd) |>
      filter(word_col != word_row)
    write_rds(q_filter, here::here("Data", str_c(group_names[i],
                                                 "_cosine.rds")))
  }
  
}


if (ind_overa != 1) {
  wt_grouped <- word_tokens |> select(id, .data[[name]])
  
  test1 <- read_rds(here::here("Data", "test1.rds")) |> left_join(wt_grouped)
  
  group_names <- unique(word_tokens[[name]])[!map_lgl(unique(
    word_tokens[[name]]), ~.x |> is.na())]
  plot_list_llr <- vector(mode = "list", length = length(group_names))
  plot_list_pmi <- vector(mode = "list", length = length(group_names))
  plot_list_lfmd <- vector(mode = "list", length = length(group_names))
  
  for (i in seq_along(1:length(group_names))) {
    
    temp1 <- test1 |> filter(.data[[name]] == group_names[i])
    ivec <- itoken(temp1$text, tokenizer = word_tokenizer, n_chunks=8,
                   ids=temp1$id)
    voc_four <- create_vocabulary(ivec, window_size = 4L)

    vec <- vocab_vectorizer(voc_four)
    
    m2 <- Collocations$new(vocabulary=voc_four, collocation_count_min = 5,
                           pmi_min = 0.0001)
    m2$partial_fit(ivec)
    
    
    pq2 <- m2$collocation_stat 
    

    mktg_mktg <- pq2 |>
      select(prefix, suffix, n_i, n_j, n_ij, pmi, llr, lfmd) |>
      filter(prefix %in% lnm$word, suffix %in% lnm$word)
    
    top_mktg <- mktg_mktg |> select(prefix, n_i) |> distinct() |>
      arrange(desc(n_i)) |> slice(1:20)
    
    
    sent_tbl_llr <- mktg_mktg |>
      select(prefix, suffix, llr) |>
      filter(prefix %in% top_mktg$prefix, suffix %in% loughran$to)  |>
      rename(from = prefix, to = suffix, weight =llr) |> 
      left_join(loughran) |> 
      mutate(weight= if_else(sentiment=="positive", weight, -1*weight)) |> 
      group_by(from) |> 
      summarise(sent_weight= sum(weight)) |> 
      ungroup() |> 
      mutate(valence= if_else(sent_weight >= 0, "positive", "negative"))
      
    
    mktg_tbl_llr <- mktg_mktg |>
      select(prefix, suffix, llr) |>
      filter(prefix %in% top_mktg$prefix, suffix %in% top_mktg$prefix)  |>
      rename(from = prefix, to = suffix, weight =llr) |> 
      left_join(sent_tbl_llr) |> 
      as_tbl_graph() |> simplify()
    
    
    V(mktg_tbl_llr)$sent_weight <- sent_tbl_llr$sent_weight
    V(mktg_tbl_llr)$valence <- sent_tbl_llr$valence
    
    
    plot_list_llr[[i]] <- ggraph(mktg_tbl_llr) +
      geom_edge_link(aes(alpha = weight), color= "black" ) +
      scale_edge_alpha_continuous("weight") +
      geom_node_point(aes(color = valence, alpha = sent_weight), size = 7) +
      geom_node_label(aes(label = name), size= 4, label.padding = .1,
                      repel = TRUE) +
      labs(title = str_c("Top 20 Marketing Terms Collocation by ",
                         group_names[i])) +
      scale_color_manual(values = c("negative" = "red",
                                    "positive"="blue")) +
      scale_alpha_continuous("sent_weight")
    
    ggsave(
      filename = here::here("Figures", str_c(group_names[i],
                                             "-LLR-collocation.png")),
      width = 12, height = 12, units = "in", limitsize = FALSE
    )
    
    # 
    # sent_tbl_pmi <- mktg_mktg |>
    #   select(prefix, suffix, pmi) |>
    #   filter(prefix %in% top_mktg$prefix, suffix %in% loughran$to)  |>
    #   rename(from = prefix, to = suffix, weight =pmi) |> 
    #   left_join(loughran) |> 
    #   mutate(weight= if_else(sentiment=="positive", weight, -1*weight)) |> 
    #   group_by(from) |> 
    #   summarise(sent_weight= sum(weight)) |> 
    #   mutate(valence= if_else(sent_weight >= 0, "positive", "negative"))
    
    # 
    # mktg_tbl_pmi <- mktg_mktg |>
    #   select(prefix, suffix, pmi) |>
    #   filter(prefix %in% top_mktg$prefix, suffix %in% top_mktg$prefix)  |>
    #   rename(from = prefix, to = suffix, weight =pmi) |> 
    #   left_join(sent_tbl) |> 
    #   as_tbl_graph() |> simplify()
    # 
    # V(mktg_tbl_pmi)$sent_weight <- sent_tbl_pmi$sent_weight
    # V(mktg_tbl_pmi)$valence <- sent_tbl_pmi$valence
    # 
    # 
    # 
    # plot_list_pmi[[i]] <- ggraph(mktg_tbl_pmi) +
    #   geom_edge_link(aes(alpha = weight), color= "black" ) +
    #   scale_edge_alpha_continuous("weight") +
    #   geom_node_point(aes(color = valence, alpha = sent_weight), size = 7) +
    #   geom_node_label(aes(label = name), size= 4, label.padding = .1,
    #                   repel = TRUE) +
    #   labs(title = str_c("Top 20 Marketing Terms PMI Collocation by ",
    #                      group_names[i])) +
    #   scale_color_manual(values = c("negative" = "red",
    #                                 "positive"="blue")) +
    #   scale_alpha_continuous("sent_weight")
    # 
    # ggsave(
    #   filename = here::here("Figures", str_c(group_names[i],
    #                                          "-PMI-collocation.png")),
    #   width = 12, height = 12, units = "in", limitsize = FALSE
    # )
    # 
    # 
    # sent_tbl_lfmd <- mktg_mktg |>
    #   select(prefix, suffix, lfmd) |>
    #   filter(prefix %in% top_mktg$prefix, suffix %in% loughran$to)  |>
    #   rename(from = prefix, to = suffix, weight = lfmd) |> 
    #   left_join(loughran) |> 
    #   mutate(weight= if_else(sentiment=="positive", weight, -1*weight)) |> 
    #   group_by(from) |> 
    #   summarise(sent_weight= sum(weight)) |> 
    #   mutate(valence= if_else(sent_weight >= 0, "positive", "negative"))
    # 
    # 
    # mktg_tbl_lfmd <- mktg_mktg |>
    #   select(prefix, suffix, lfmd) |>
    #   filter(prefix %in% top_mktg$prefix, suffix %in% top_mktg$prefix)  |>
    #   rename(from = prefix, to = suffix, weight = lfmd) |> 
    #   left_join(sent_tbl) |> 
    #   as_tbl_graph() |> simplify()
    # 
    # V(mktg_tbl_lfmd)$sent_weight <- sent_tbl_lfmd$sent_weight
    # V(mktg_tbl_lfmd)$valence <- sent_tbl_lfmd$valence
    # 
    # 
    # plot_list_lfmd[[i]] <- ggraph(mktg_tbl_lfmd) +
    #   geom_edge_link(aes(alpha = weight), color= "black" ) +
    #   scale_edge_alpha_continuous("weight") +
    #   geom_node_point(aes(color = valence, alpha = sent_weight), size = 7) +
    #   geom_node_label(aes(label = name), size= 4, label.padding = .1,
    #                   repel = TRUE) +
    #   labs(title = str_c("Top 20 Marketing Terms LFMD Collocation by ",
    #                      group_names[i])) +
    #   scale_color_manual(values = c("negative" = "red",
    #                                 "positive"="blue")) +
    #   scale_alpha_continuous("sent_weight")
    # 
    # ggsave(
    #   filename = here::here("Figures", str_c(group_names[i],
    #                                          "-LFMD-collocation.png")),
    #   width = 12, height = 12, units = "in", limitsize = FALSE
    # )
    # 
   # qq <-  gcor(asNetwork(mktg_tbl_llr), asNetwork(mktg_tbl_lfmd))
   #  
   #  qap <- qaptest(dat=list(asNetwork(mktg_tbl_llr), asNetwork(mktg_tbl_pmi),
    #asNetwork(mktg_tbl_lfmd)),
   #                 FUN = gcor, g1=1:3, g2=1:3 , reps=1000)
   #  
   #  write_rds(qap, here::here("Data", str_c(group_names[i], "-QAP.png")))
  }
  # if (ind_sectr == 1) {
  #   ( plot_list[[1]] | plot_list[[2]]) / 
  #     ( plot_list[[3]] | plot_list[[4]]) / 
  #     ( plot_list[[5]] | plot_list[[6]]) / 
  #     ( plot_list[[7]] | plot_list[[8]]) / 
  #     ( plot_list[[9]] | plot_list[[10]]) / 
  #     ( plot_list[[11]]) / 
  #   # Specify plot dimensions.
  #   width <- 36; height <- 72
  # }
  # if (ind_group == 1) {
  #   ( plot_list[[1]] | plot_list[[2]] | plot_list[[3]] ) /
  #     ( plot_list[[4]] | plot_list[[5]] | plot_list[[6]] ) /
  #     ( plot_list[[7]] | plot_list[[8]] | plot_list[[9]] ) /
  #     ( plot_list[[10]] | plot_list[[11]] | plot_list[[12]] ) /
  #     ( plot_list[[13]] | plot_list[[14]] | plot_list[[15]] ) /
  #     ( plot_list[[16]] | plot_list[[17]] | plot_list[[18]] ) /
  #     ( plot_list[[19]] | plot_list[[20]] | plot_list[[21]] )
  #   # Specify plot dimensions.
  #   width <- 36; height <- 36
  # }
}




