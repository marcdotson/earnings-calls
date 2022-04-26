# Tokenizing --------------------------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)

# Read data.
call_data <- read_rds(here::here("Data", "call_data.rds"))
joint_data <- read_rds(here::here("Data", "joint_data.rds"))

#Notes on packages:
#The SMLTA book incorporates several different packages which supplement the tidytext package:
#tokenizer is, surprise surprise, a tokenizer
#stopwords package, which may or may not already be a part of tidytext
#SnowballC does stemming
#quanteda creates and manages sparse matrices
#slider creates slide windows; used for word embeddings
#widyr does word embeddings; supposedly this package isn't good for academic purposes
#furrr enables parallel processing
#text2vec, word2vec, and FastText provide alternative options for embeddings
#edgar has the LMMasterDictionary

# We need to tokenize based on what? Some things to consider:
# - You may want to include title and text together.
# - Documents defined by earnings call or year/quarter or year.
# - Keep all the group information as you tokenize.

# Los Tokens
text_tokens <- text_tibble %>% unnest_tokens(word, text, token = "words")

# Remove Stop Words -------------------------------------------------------
# - stopwords data frame needs to be looked at in tidytext.
# - specialized stopwords .txt file provided by the dictionary authors.

# Stemming ----------------------------------------------------------------

# Word Embeddings ---------------------------------------------------------

