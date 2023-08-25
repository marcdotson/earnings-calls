# My installation of Python is a bit messed up I think, and I need to update
# RStudio as well. Some of the libraries below might be redundant or irrelevant,
# so this is really where the environment functionality Python supports would come
# in handy, as I've had to reinstall/update a few of my libraries to get this 
# all to work. 

import pandas as pd
pd.options.mode.chained_assignment = None
import lib
import numpy as np
import gensim
import re
import nltk
from gensim.models import word2vec
from gensim.scripts.glove2word2vec import glove2word2vec
import gensim.models.keyedvectors as word2vec
from gensim.models import Word2Vec, KeyedVectors

# Half of this code is from Carly, the other half is from me.
# First we load in the pretrained embeddings and convert them to a 
# standardized format. Then we take those embeddings and join them together 
# with the words found in our corpus. The join could probably be done more 
# efficiently in R, but it's a good deal easier to work with word2vec models in
# Python than R. 

# Load word2vec model downloaded from here:
# https://developer.syn.co.in/tutorial/bot/oscova/pretrained-vectors.html
# Pulling out the embeddings, I might've renamed this file but it should be 300d
model_w2v = word2vec.KeyedVectors.load_word2vec_format('Data/w2v-vectors-negative.bin', binary=True) 


# Import GloVe model, then reformat as a word2vec file, then load it back in

# GloVe embeddings are saved as .txt files, specify path to GloVe below
glove_input_file = 'Data/glove.42B.300d.txt'

# Name of output file
word2vec_output_file = 'Data/glove.42B.300d.txt.word2vec'

# Convert GloVe model to word2vec format
glove2word2vec(glove_input_file, word2vec_output_file)

# Load the converted model
model_glove = KeyedVectors.load_word2vec_format(word2vec_output_file, binary=False)

# Now we load in our wordlist from our corpus. I had this file hanging around,
# which looks to be the correct file
nlist = pd.read_csv('Data/tot_count.csv')


# Join list of words to their embeddings for word2vec
labels_w2v = []
tokens_w2v = []
for i in range(len(nlist['word'])):
    word = nlist.iloc[i, 0] # take the word at row i
    try:
      tokens_w2v.append(model_w2v[word]) # try to append the embeddings of the word to a list
      labels_w2v.append(word) # if that succeeds, append the actual word to a list
    except:
      pass # if not, skip to next 
tokens_w2v = np.array(tokens_w2v)

w2v = pd.DataFrame(tokens_w2v) # turn embeddings into a dataframe
w2v['word'] = labels_w2v # add the the words as a column
w2v.to_csv('w2v.csv') # save as csv


# Ditto for GloVe
labels_glove = []
tokens_glove = []
for i in range(len(nlist['word'])):
    word = nlist.iloc[i, 0]
    try:
      tokens_glove.append(model_glove[word])
      labels_glove.append(word)
    except:
      pass
tokens_glove = np.array(tokens_glove)

glove = pd.DataFrame(tokens_glove)
glove['word'] = labels_glove
glove.to_csv('glove.csv')

