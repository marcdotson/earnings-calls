# Import libraries and functions.
import nltk
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer

from gensim.models import Word2Vec, KeyedVectors
from gensim.scripts.glove2word2vec import glove2word2vec
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA

import matplotlib.pyplot as plt
import numpy as np

import pandas as pd
import warnings
warnings.filterwarnings("ignore")

# Let's define two different simple corpora for customer feedback
corpus_1 = [
    "The product quality was great",
    "Shipping was very fast",
    "Customer service was not helpful",
    "Loved the product",
    "Shipping was slow",
    "Excellent customer service",
    "Product quality was poor",
    "Fast shipping",
    "Customer service could be better",
    "Very happy with the product"
]

# Define the text cleaning function
def clean_text(text):
    # Case folding
    text = text.lower()
    
    # Tokenization
    words = nltk.word_tokenize(text)
    
    # Stopwords removal
    stop_words = stopwords.words('english')
    words = [word for word in words if word not in stop_words]
    
    # Lemmatization
    lemmatizer = WordNetLemmatizer()
    words = [lemmatizer.lemmatize(word) for word in words]
    
    return words

tokens = [clean_text(doc) for doc in corpus_1]

corpus_1 # List of strings.
tokens   # List of list of tokens.

# WHAT IS THIS?
# %%time

# WHY CONVERT GLOVE INTO WORD2VEC?
# If we have the GloVe model downloaded and we'll convert Glove vectors in text format into the word2vec text format:
glove_input_file = '/Users/marcdotson/Box Sync/4 Exploring Marketing Term Usage in Earnings Calls/data/glove/glove.6B.50d.txt'

word2vec_output_file = '/Users/marcdotson/Box Sync/4 Exploring Marketing Term Usage in Earnings Calls/data/glove/glove.6B.50d.txt.word2vec'
glove2word2vec(glove_input_file, word2vec_output_file)

# load the converted model
model_glove = KeyedVectors.load_word2vec_format(word2vec_output_file, binary=False)

def average_word_vectors(words, model, vocabulary, num_features):
    feature_vector = np.zeros((num_features,),dtype="float64") # initialize feature_vectors of num_features length as 0s
    nwords = 0.
    
    for word in words:
        if word in vocabulary: 
            nwords = nwords + 1.
            feature_vector = np.add(feature_vector, model[word]) # add up the feature vectors for each word in vocab
    
    if nwords:
        feature_vector = np.divide(feature_vector, nwords) # compute the average vector
        
    return feature_vector

def averaged_word_vectorizer(corpus, model, num_features):
    if isinstance(model, Word2Vec):
        vocabulary = set(model.wv.index_to_key)
        features = [average_word_vectors(tokenized_sentence, model.wv, vocabulary, num_features)
                        for tokenized_sentence in corpus]
    else:  # this part is for GloVe model, which is loaded as KeyedVectors
        vocabulary = set(model.index_to_key)
        features = [average_word_vectors(tokenized_sentence, model, vocabulary, num_features)
                        for tokenized_sentence in corpus] # computes the averge vectors for each document, returns array

    return np.array(features)

# Get document level embeddings
glove_feature_array = averaged_word_vectorizer(corpus=tokens, model=model_glove, num_features=50)

glove_feature_array.shape

# Building TF-IDF matrix
vectorizer = TfidfVectorizer()
X_tfidf = vectorizer.fit_transform(corpus_1)

X_tfidf.shape

# Now, we will perform KMeans clustering
kmeans_glove = KMeans(n_clusters=3, random_state=42).fit(glove_feature_array)
kmeans_tfidf = KMeans(n_clusters=3, random_state=42).fit(X_tfidf)

# Function to plot PCA 
def plot_pca(data, labels, model_name):
    pca = PCA(n_components=2)
    scatter_plot_points = pca.fit_transform(data)

    colors = ["red", "blue", "green"]

    plt.scatter(scatter_plot_points[:, 0], scatter_plot_points[:, 1], c=[colors[i] for i in labels])

    # add labels
    for i, label in enumerate(labels):
        plt.text(scatter_plot_points[i, 0], scatter_plot_points[i, 1], str(i))

    plt.title('PCA plot for '+ model_name)
    plt.show()

# Plotting PCA plots
plot_pca(glove_feature_array, kmeans_glove.labels_, 'GloVe')
plot_pca(X_tfidf.toarray(), kmeans_tfidf.labels_, 'TF-IDF')

# Convert corpus to a DataFrame
df = pd.DataFrame(corpus_1, columns=['text'])

# Add KMeans cluster labels
df['cluster_glove'] = kmeans_glove.labels_
df['cluster_tfidf'] = kmeans_tfidf.labels_

df

