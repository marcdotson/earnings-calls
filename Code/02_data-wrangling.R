# Load packages
library(tidyverse)
library(tidytext)
# library(textdata)
# library(topicmodels)
# library(dbplyr)
# library(DBI)
library(lubridate)


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


# Put the text in a tibble

text_tibble <- tibble(text = text)

# Extracting basic information from the beginning. If every single file is the same
# format, this should work. If it fails, we can get spicier with it.

# Los Tokens
text_tokens <- text_tibble %>% unnest_tokens(word, text, token = "words")


#
info_tibble <- tibble(
  Company = str_sub(text_tibble[1,1], start = 23L, end = str_locate(text_tibble[1,1], " Earnings")[1,1]) %>% str_trim(),
  Quarter = str_sub(text_tibble[1,1],start = 16L, end = 16L),
  Year = str_sub(text_tibble[1,1],start = 18L, end = 21L),
  Word_Count = str_sub(text_tibble[2,1], start = 15L, end = -7L),
  Date = dmy(str_sub(text_tibble[3,1], start=15L, end =-1L)) 
)  %>% separate(Date, into=c("Year", "Month", "Day"), sep = "-")


# I had the genius idea of making this a function instead of not making it a function
# Tentative idea for this: function to pull a text file from AWS, get all
# relevant summary info, then put it into a tibble. Another idea is to have the
# function sort all of the companies into separate tibbles.


# I used this AWS database as a proof of concept
con <- dbConnect(
  RPostgreSQL::PostgreSQL(),
  dbname = "analyticsdb",
  host = "analyticsdb.ccutuqssh92k.us-west-2.rds.amazonaws.com",
  port = 55432,
  user = "quantmktg",
  password = rstudioapi::askForPassword("Database password")
)
x <- dbListTables(con)
# tbl(con, "roomba_reviews") %>%
#   collect()
# 
# q <- tbl(x[[1]])

# Tentative idea for this: function to pull a text file from AWS, get all
# relevant summary info, then put it into a tibble. Another idea is to have the
# function sort all of the companies into separate tibbles.


earnings_call_wrangler <- function(x) {
  y <- dbListTables(x)
  file_name <- NA
  file_imp <- NA
  summary_tib <- tibble(
    Company = NA,
    Quarter = NA,
    Year = NA,
    Word_Count = NA
  )
  for(i in 1:length(y)) {
    file_name <- y[[i]] # storing file name
    file_imp <- tbl(x, file_name) %>% collect() # importing from database
    text_tibble <- tibble(file_imp=text)
    info_tibble <- tibble(
      Company = str_sub(text_tibble[1,1], start = 23L, end = str_locate(text_tibble[1,1], " Earnings")[1,1]) %>% str_trim(),
      Quarter = str_sub(text_tibble[1,1],start = 16L, end = 16L),
      Year = str_sub(text_tibble[1,1],start = 18L, end = 21L),
      Word_Count = str_sub(text_tibble[2,1], start = 15L, end = -7L),
      Date = dmy(str_sub(text_tibble[3,1], start=15L, end =-1L)) 
    )  %>% separate(Date, into=c("Year", "Month", "Day"), sep = "-")
    summary_tib %>% bind_rows(info_tibble)
  }
  
}

earnings_call_wrangler(con)


dbDisconnect(con)
