library(tidyverse)
library(tidytext)
library(furrr)
library(text2vec)
library(wordsalad)

set.seed(42)

# word_tokens should be on the google drive folder. This takes a minute to run,
# so it might be worth saving the output so you don't have to rerun it. 
word_tokens <- read_rds(here::here("Data", "word_tokens.rds")) %>% 
  transmute(text= future_map_chr(words, ~paste0(.x, collapse = " ")),
            id = id) %>% 
  # Getting rid of the c("...", "...") stuff from pasting everything back together
  mutate(text= str_remove_all(text, "[\"\\(\\),]") %>% str_sub(2, -1))


# Run this please
w2v_model = word2vec(word_tokens$text, dim=100, min_count = 200, threads = 8L, composition = "matrix")
# I get the following error:
# Training failed: fileMapper: file C:\Users\Will\AppData\Local\Temp\Rtmpe0Bp2y\textspace_8df05683ac5.txt is empty, nothing to read
# Error in `Encoding<-`(`*tmp*`, value = encoding) : 
#   a character vector argument expected
# In addition: Warning message:
#   In file.remove(file_train) :
#   cannot remove file 'C:\Users\Will\AppData\Local\Temp\Rtmpe0Bp2y\textspace_8df05683ac5.txt', reason 'Permission denied'
# 
# I wasn't ever able to resolve this issue; I had to figure out a way to work around it.
# The problem seemed to be the size of the temp .txt file that it was trying to save,
# there is either an error while writing it or the function moves to loading it 
# before it properly saves
# 
# My work-around was to modify the source code for the word2vec::word2vec function
# by allowing you to reference a previously trained model instead of creating
# a new one from scratch, which hopefully means you can iterate the training
# process across multiple datasets. So, as I did with every memory-related problem,
# I split up the data and ran the model four times, and if everything worked
# as I hope, it used the embeddings from the previous iteration as the start 
# for the next. 
#
# However, since the point of all this was to compare it to GLoVE, it would be 
# far more preferable to not have to do all that, as I didn't have to split it
# when I did the GLoVE embeddings.

# Also run this, as I didn't use wordsalad originally, and only found it after
# the fact
glove_model = glove(word_tokens$text, dim=100, min_count = 200, threads = 8L, composition = "matrix")