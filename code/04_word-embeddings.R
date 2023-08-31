# Pre-Trained Word Embeddings ---------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)

# Import word tokens.
word_tokens <- read_rds(here::here("data", "word_tokens.rds")) |> 
  select(id, words) |> 
  unnest(cols = words)

word_tokens

# Specify number of embeddings/dimensions and import pre-trained word embeddings 
# (data downloaded from https://nlp.stanford.edu/projects/glove/).
n_embeddings <- 50
glove_embeddings <- read_delim(
  here::here("data", "glove", str_c("glove.6B.", n_embeddings, "d.txt")),
  delim = " ",
  # na = c("", "NA", "\""), # Issue with the glove embeddings?
  col_names = c("word", str_c("emb", 1:n_embeddings))
)

glove_embeddings

# Join word tokens and embeddings.
word_embeddings <- word_tokens |> 
  inner_join(glove_embeddings, by = "word")

# Write pre-trained word embeddings.
write_rds(
  word_embeddings,
  here::here("data", str_c("word_embeddings-glove_", n_embeddings, ".rds"))
)

# Pre-Trained Transformer -------------------------------------------------

