# GloVe Word Embeddings ---------------------------------------------------

# This is all based on the text2vec package, and is very similar to what we did with the collocations. 

library(tidyverse)
library(tidytext)
library(furrr)
library(text2vec)

# GLoVE is actually in rsparse https://cran.r-project.org/web/packages/rsparse/rsparse.pdf

# Copy paste from collocation RScript

# Reload recombined tokens
word_tokens <- read_rds(here::here("Data", "word_tokens.rds"))

# Recombine words for text2vec format
# test1 isn't a great name, but this takes awhile to run, and I don't want to change it.
test1 <- word_tokens |>
  transmute(text= future_map_chr(words, ~paste0(.x, collapse = " ")),
            id = id)

# Saving an intermediate step in case there are memory constraints later

# write_rds(test1, here::here("Data", "test1.rds"))
# test1 <- read_rds(here::here("Data", "test1.rds"))

# text2vec corpus creation--specify text, tokenizer, number of chunks, document
# id


ivec <- itoken(test1$text, tokenizer = word_tokenizer, n_chunks=8, ids=test1$id)

# text2vec vocabulary, used in later functions
vocab <- create_vocabulary(ivec)

# Filtering vocabulary by words with low word count
vocab <- prune_vocabulary(vocab, term_count_min = 100L) # Definitely subjective choice

# Vectorizer helps computations
vec <- vocab_vectorizer(vocab)

# Create a term co-occurrence matrix
tcm_5 <- create_tcm(ivec, vec, skip_grams_window = 5L)

# Again, saving an intermediate step
# write_rds(tcm_5, here::here("Data", "tcm_5.rds"))


glove = GlobalVectors$new(rank = 100, x_max = 10)
wv_main = glove$fit_transform(tcm_5, n_iter = 10, convergence_tol = 0.01, n_threads = 8)


wv_context = glove$components
word_vectors = wv_main + t(wv_context)


write_rds(word_vectors, here::here("Data", "glove_5.rds"))

word_vectors = read_rds(here::here("Data", "glove_5.rds"))



# Word2Vec Word Embeddings ------------------------------------------------

# https://cran.r-project.org/web/packages/word2vec/readme/README.html

library(word2vec)
library(udpipe) # This package is pretty similar to text2vec, and is used in
# the documentation for the word2vec package to help prepare data

# Reusing the same file, should be fine
test1 <- read_rds(here::here("Data", "test1.rds")) %>% 
  # Getting rid of the c("...", "...") stuff from pasting everything back together
  mutate(text= str_remove_all(text, "[\"\\(\\),]") %>% str_sub(2, -1)) %>% 
  rename(doc_id = id)

set.seed(42)


slice1 = test1 %>% slice(1:32000)
slice2 = test1 %>% slice(32001:64000)
slice3 = test1 %>% slice(64001:96000)
slice4 = test1 %>% slice(96001:130527)
x1 = txt_clean_word2vec(slice1$text)
x2 = txt_clean_word2vec(slice2$text)
x3 = txt_clean_word2vec(slice3$text)
x4 = txt_clean_word2vec(slice4$text)

# Going to try to replicate GLoVE as best I can
model <- word2vec(x = x1 , type = "cbow", dim = 100, window =5, iter = 10, threads = 8, min_count = 25) 
write.word2vec(model, here::here("Data", "model1.bin"))

# I'm not sure if this worked or not, but I added an argument where the model
# used is the previous model, essentially allowing you to iterate the training
# process. Not sure if this is legit though
custom_w2v = function (x, type = c("cbow", "skip-gram"), dim = 50, window = ifelse(type == 
                                                                                     "cbow", 5L, 10L), iter = 5L, lr = 0.05, hs = FALSE, negative = 5L, 
                       sample = 0.001, min_count = 5L, split = c(" \n,.-!?:;/\"#$%&'()*+<=>@[]\\^_`{|}~\t\v\f\r", 
                                                                 ".\n?!"), prev_model_name= "model.bin", stopwords = character(), threads = 1L, encoding = "UTF-8", 
                       ...) {
  type <- match.arg(type)
  stopw <- stopwords
  prev_model_name <- as.character(prev_model_name)
  model <- here::here("Data", prev_model_name)
  if (length(stopw) == 0) {
    stopw <- ""
  }
  file_stopwords <- tempfile()
  filehandle_stopwords <- file(file_stopwords, open = "wt", 
                               encoding = encoding)
  writeLines(stopw, con = filehandle_stopwords)
  close(filehandle_stopwords)
  on.exit({
    if (file.exists(file_stopwords)) unlink(file_stopwords, recursive = T)
  })
  if (length(x) == 1) {
    file_train <- x
  }
  else {
    file_train <- tempfile(pattern = "textspace_", fileext = ".txt")
    on.exit({
      if (file.exists(file_stopwords)) unlink(file_stopwords, recursive = T)
      if (file.exists(file_train)) unlink(file_train, recursive = T)
    })
    filehandle_train <- file(file_train, open = "wt", encoding = encoding)
    writeLines(text = x, con = filehandle_train)
    close(filehandle_train)
  }
  w2v_train <- get("w2v_train", envir=asNamespace("word2vec"))
  min_count <- as.integer(min_count)
  dim <- as.integer(dim)
  window <- as.integer(window)
  iter <- as.integer(iter)
  sample <- as.numeric(sample)
  hs <- as.logical(hs)
  negative <- as.integer(negative)
  threads <- as.integer(threads)
  iter <- as.integer(iter)
  lr <- as.numeric(lr)
  skipgram <- as.logical(type %in% "skip-gram")
  split <- as.character(split)
  model <- w2v_train(trainFile = file_train, modelFile = model, 
                     stopWordsFile = file_stopwords, minWordFreq = min_count, 
                     size = dim, window = window, sample = sample, withHS = hs, 
                     negative = negative, threads = threads, iterations = iter, 
                     alpha = lr, withSG = skipgram, wordDelimiterChars = split[1], 
                     endOfSentenceChars = split[2], ...)
  model$data$stopwords <- stopwords
  model
}


model2 <- custom_w2v(x = x2 , type = "cbow", dim = 100, window =5, iter = 10, threads = 8, min_count = 25) 

write.word2vec(model2, here::here("Data", "model2.bin"))

model3 <- custom_w2v(x = x3 , type = "cbow", dim = 100, window =5, iter = 10, threads = 8, min_count = 25, prev_model_name = "model2.bin") 

write.word2vec(model3, here::here("Data", "model3.bin"))

model4 <- custom_w2v(x = x4 , type = "cbow", dim = 100, window =5, iter = 10, threads = 8, min_count = 25, prev_model_name = "model3.bin") 

write.word2vec(model4, here::here("Data", "model4.bin"))


# Vector Comparison -------------------------------------------------------

# Import word2vec final vectors
w2v_vectors = read.wordvectors(here::here("Data", "model4.bin"))

# Import Glove vectors
glove_vectors = read_rds(here::here("Data", "glove_5.rds"))

# Import wordlist
word_list = read_csv(here::here("Data", "MarkDict.csv"))

# Filter out only marketing words from wordlist

# W2V filtered down words a bit more aggressively, so to compare vectors
# between matrices, we will only use those present in both Glove and W2V
marketing_words = word_list %>% 
  filter(marketing==1, word %in% rownames(w2v_vectors) & word %in% rownames(glove_vectors)) %>% 
  select(word)

# Filter out only marketing words 
w2v_marketing = w2v_vectors %>% 
  as_tibble(rownames = "word") %>% 
  semi_join(marketing_words) %>% 
  arrange(word) %>% 
  column_to_rownames(var = "word") %>% 
  as.matrix()

# Filtering out marketing words
glove_marketing = glove_vectors %>% 
  as_tibble(rownames = "word") %>% 
  semi_join(marketing_words) %>% 
  arrange(word) %>% 
  column_to_rownames(var = "word") %>% 
  as.matrix()
# The embedding matrices are quite large, so doing anything with the full-sized
# matrix is ill-advised, as it is extremely memory intensive

w2v_sim = sim2(w2v_marketing, method = "cosine", norm = "l2")

glove_sim = sim2(glove_marketing, method = "cosine", norm = "l2")

w2v_glove_sim = psim2(w2v_sim, glove_sim, method = "cosine", norm = "l2")

