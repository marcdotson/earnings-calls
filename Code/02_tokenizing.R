# Tokenize and Remove Stop Words ------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)

# Import call data.
call_data <- read_rds(here::here("Data", "call_data.rds")) |> 
  mutate(
    title_text = str_c(title, text, " "),                 # Combine title and text.
    title_text = str_replace_all(text, "[:punct:]", " ")  # Strip punctuation to deal with contractions.
  ) |> select(id, title_text)                             # Select only id and text to reduce memory demand


# Mutate function takes a bit to run, so I saved it
# write_rds(call_data, here::here("Data", "temp_call.rds"))
# call_data <- read_rds(here::here("Data", "temp_call.rds"))

# Import L&M generic stop words.
generic_stopwords <- read_rds(here::here("Data", "generic_stopwords_long.rds"))

generic_stopwords

# Tokenize call data in sets to avoid memory loss and limits.
num_splits <- 30
for (i in seq_along(1:num_splits)) {
  # Specify the start and end rows for each of the sets.
  start <- round(nrow(call_data)/num_splits * (i - 1)) + 1
  end <- round(nrow(call_data)/num_splits * i)
  
  # Name assignment is based on index variable.
  tokens_name <- str_c("tokens_", i, ".rds")
  
  # Tokenize and assign to a separate object each iteration.
  tokens <- call_data |> 
    # Chunking the data
    slice(start:end) |> 
    # Strip punctuation to deal with contractions.
    unnest_tokens(word, title_text, strip_punct = TRUE) |> 
    # Clean up contractions.
    mutate(
      word = case_when(
        word == "ll" ~ "will",
        word == "ve" ~ "have",
        word == "t" ~ "not",
        word == "d" ~ "had",
        word == "s" ~ "is",
        word == "re" ~ "are",
        TRUE ~ word
      )
    ) |> 
    # Remove generic stop words.
    anti_join(generic_stopwords) |> 
    # Save tokens with associated id
    write_rds(here::here("Data", tokens_name))

    # Remove objects to keep memory demand low
    rm(tokens, tokens_name)
}


# tokens_1

# Bind Data ---------------------------------------------------------------
# Remove data that no longer needs to be held in memory.
rm(call_data, generic_stopwords)


# Reimport complete call_data except for text.
call_data <- read_rds(here::here("Data", "call_data.rds")) |> 
  select(
    id, gvkey, sector, group, industry, sub_industry, 
    call_date, year, quarter, revenue, title)

# Create word_tokens tibble
word_tokens <- NULL

# Binding separate sets
for (i in 1:num_splits) {
  # Recreate saved file names
  tokens_name <- str_c("tokens_", i, ".rds")
  
  # Import saved files
  tokens <- read_rds(here::here("Data", tokens_name)) |>
    group_by(id) |> 
    # Nesting words by id, hopefully reducing memory demand. This can always be undone with unnest()
    nest(words = c(word)) |> 
    # Rejoin firm data
    left_join(call_data, by = "id")
  
  # Bind chunked data to main data frame
  word_tokens <- word_tokens |>
    bind_rows(tokens)
  
  # Remove rows from call data that have been joined. This reduces memory demand and highlights any 
  # documents which might've been missed
  call_data <- call_data |> anti_join(word_tokens, by = "id")
  
  # Deleting chunked token files
  unlink(here::here("Data",tokens_name))
}

# Bind the separate sets.
# word_tokens <- NULL
# for (i in seq_along(1:num_splits)) {
#   word_tokens <- bind_rows(word_tokens, get(str_c("tokens_", i)))
# }

word_tokens

# Remove data that no longer needs to be held in memory.
# rm(list = setdiff(ls(), "word_tokens"))

# Write the word tokens.
write_rds(word_tokens, here::here("Data", "word_tokens.rds"))



word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))

