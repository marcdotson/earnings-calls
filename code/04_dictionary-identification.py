# Import libraries
import pandas as pd
from kneed import KneeLocator
from sklearn.cluster import MiniBatchKMeans
import time
import numpy as np
import matplotlib.pyplot as plt
from sklearn.cluster import MiniBatchKMeans
from sklearn.manifold import TSNE

import warnings

# Ignore all warnings
warnings.filterwarnings("ignore")

# Reading the file into a pandas dataframe
df = pd.read_csv("data/word_tokens_sample.txt", sep=" ", names=["id", "word"])

# Removing the redundant header row
df = df[df["id"] != "id"]

# Resetting the index
df.reset_index(drop=True, inplace=True)

# Displaying the first few rows of the dataframe
df.head() 

# Reading the GloVe embeddings file and extracting word-to-embedding mappings
embeddings_dict = {}

with open("data/glove/glove.6B.50d.txt", "r") as file:
    for line in file:
        values = line.split()
        word = values[0]
        vector = list(map(float, values[1:]))
        embeddings_dict[word] = vector

# Checking the first few entries in the embeddings_dict
list(embeddings_dict.items())[:5]

# Mapping the words in the dataframe to their corresponding embeddings
df['embedding'] = df['word'].map(embeddings_dict)

# Dropping rows where there is no corresponding embedding
df_embeddings = df.dropna(subset=['embedding'])

# Checking the number of words successfully mapped to embeddings
len(df_embeddings)

################################################
# 974552 words via R, 985960 words via Python?
################################################

# Creating the word_to_vec dictionary from the dataframe
word_to_vec = dict(zip(df_embeddings['word'], df_embeddings['embedding']))

# Extracting the embeddings for clustering
embeddings_matrix = list(word_to_vec.values())

# Running the fast k-means clustering (MiniBatchKMeans) and timing the execution
start_time = time.time()

kmeans = MiniBatchKMeans(n_clusters=3, random_state=0, batch_size=1000).fit(embeddings_matrix)

end_time = time.time()

# Calculating the time taken
time_taken = end_time - start_time
time_taken

################################################
# Mini Batch: https://scikit-learn.org/stable/modules/clustering.html#mini-batch-kmeans
################################################

# Define the range of k values to test
k_values = range(2, 30)
inertias = []

# Run MiniBatchKMeans for each k
for k in k_values:
    kmeans = MiniBatchKMeans(n_clusters=k, random_state=0, batch_size=1000).fit(embeddings_matrix)
    inertias.append(kmeans.inertia_)

# Plot the results on an elbow plot
plt.figure(figsize=(10, 6))
plt.plot(k_values, inertias, 'o-')
plt.xlabel('Number of clusters (k)')
plt.ylabel('Inertia (Sum of Squared Distances)')
plt.title('Elbow Plot for Optimal k')
plt.grid(True)
plt.show()

kn = KneeLocator(k_values, inertias, curve='convex', direction='decreasing')
elbow_point = kn.knee
elbow_point

################################################
# KneeLocator: https://scikit-learn.org/stable/modules/clustering.html#mini-batch-kmeans
################################################

kmeans = MiniBatchKMeans(n_clusters=12, random_state=0, batch_size=1000).fit(embeddings_matrix)

# Ensure your embeddings matrix is a NumPy array
embeddings_matrix_np = np.array(embeddings_matrix)

# Using t-SNE for dimensionality reduction to 2D
embeddings_2d = TSNE(n_components=2, random_state=0).fit_transform(embeddings_matrix_np[:10000])  # Limiting to first 10,000 samples for speed

# Getting the cluster labels for the points we're visualizing
labels = kmeans.predict(embeddings_matrix[:10000])

# Plotting the clusters
plt.figure(figsize=(10, 8))
scatter = plt.scatter(embeddings_2d[:, 0], embeddings_2d[:, 1], c=labels, cmap='viridis', alpha=0.5)
plt.title("Clusters of Word Embeddings (via t-SNE)")
plt.xlabel("t-SNE Dimension 1")
plt.ylabel("t-SNE Dimension 2")
plt.colorbar(scatter)
plt.show()

# Assuming words is a list of your words corresponding to embeddings_matrix_np
words = list(word_to_vec.keys())

# Number of top words per cluster to display
top_n = 10

# Find the top N words closest to each centroid
for i, centroid in enumerate(kmeans.cluster_centers_):
    distances = np.linalg.norm(embeddings_matrix_np - centroid, axis=1)
    closest_words_idx = np.argsort(distances)[:top_n]
    closest_words = [words[idx] for idx in closest_words_idx]

    print(f"Cluster {i + 1}: {', '.join(closest_words)}")

################################################
# Top words: Based on nearness to each centroid. What about something like tf-idf?
# ChatGPT: Just fed those terms into ChatGPT to summarize.
################################################

kmeans = MiniBatchKMeans(n_clusters=25, random_state=0, batch_size=1000).fit(embeddings_matrix)

# Ensure your embeddings matrix is a NumPy array
embeddings_matrix_np = np.array(embeddings_matrix)

# Using t-SNE for dimensionality reduction to 2D
embeddings_2d = TSNE(n_components=2, random_state=0).fit_transform(embeddings_matrix_np[:10000])  # Limiting to first 10,000 samples for speed

# Getting the cluster labels for the points we're visualizing
labels = kmeans.predict(embeddings_matrix[:10000])

# Plotting the clusters
plt.figure(figsize=(10, 8))
scatter = plt.scatter(embeddings_2d[:, 0], embeddings_2d[:, 1], c=labels, cmap='viridis', alpha=0.5)
plt.title("Clusters of Word Embeddings (via t-SNE)")
plt.xlabel("t-SNE Dimension 1")
plt.ylabel("t-SNE Dimension 2")
plt.colorbar(scatter)
plt.show()

# Assuming words is a list of your words corresponding to embeddings_matrix_np
words = list(word_to_vec.keys())

# Number of top words per cluster to display
top_n = 10

# Find the top N words closest to each centroid
for i, centroid in enumerate(kmeans.cluster_centers_):
    distances = np.linalg.norm(embeddings_matrix_np - centroid, axis=1)
    closest_words_idx = np.argsort(distances)[:top_n]
    closest_words = [words[idx] for idx in closest_words_idx]

    print(f"Cluster {i + 1}: {', '.join(closest_words)}")

