# Load Data ---------------------------------------------------------------

library(jsonlite)
library(data.table)

# Read the JSON file and extract embeddings column
df <- fromJSON("./data/data.json")

# Add song_id column
df$song_id <- 1:length(df$track_name)
embeddings <- df$embeddings

# Convert df to a data.table and save as csv
df <- as.data.table(df)
fwrite(df, "./data/music.csv")

# Compute Embeddings Similarity  -----------------------------------------------

library(parallel)
library(doParallel)
library(pbapply)
library(lsa)

# Initialize an empty matrix with the desired dimensions
similarity_matrix <- matrix(nrow = length(embeddings), ncol = length(embeddings))

# Define a function to calculate the cosine similarity between two embeddings
cosine_similarity <- function(i, j) {
  cosine(embeddings[[i]], embeddings[[j]])
}

# Set the number of cores to use
cl <- makeCluster(4)
clusterExport(cl, "cosine")
clusterExport(cl, "cosine_similarity")
clusterExport(cl, "embeddings")

# Use parLapply to calculate the cosine similarities in parallel
similarity_matrix <- pblapply(cl=cl, X=1:length(embeddings), FUN=function(i) {
  sapply(1:length(embeddings), function(j) {
    cosine_similarity(i, j)
  })
})

similarity_matrix <- do.call(cbind, similarity_matrix)
stopCluster(cl)

tri_matrix <- similarity_matrix * upper.tri(similarity_matrix, diag = FALSE)
saveRDS(tri_matrix, './data/tri_matrix')

# Thresholding --------------------------------------------------------------

# Find the indices of the elements in M that are greater than t
idx = tri_matrix > 0.3
indices = which(idx, arr.ind = TRUE)

# Extract the row and column indices of the elements
keep_nodes <- union(indices[, 1], indices[, 2])

# Create a data frame with the indices
edges <- data.table(from = indices[, 1], to = indices[, 2], cosine = round(tri_matrix[idx],  2))
fwrite(edges, "./data/edges.csv")

# Create Graph --------------------------------------------------------------

library(igraph)

numeric_cols <- c('len', 'dating', 'violence', 'world/life', 'night/time', 'shake the audience', 'family/gospel', 
                  "romantic", "communication", "obscene", "music", "movement/places", "light/visual perceptions",
                  "family/spiritual", "like/girls", "sadness", "feelings", "danceability", "loudness", "acousticness",
                  "instrumentalness", "valence", "energy" )

keep_cols <- names(df)[-5]

#round numeric columns
rounded_numeric_cols <- lapply(df[, numeric_cols, with = FALSE], FUN=round, digits = 3)
df[, (numeric_cols) := rounded_numeric_cols]

#keep only nodes with degree > 0, nodes connected with cos(theta) > 0.7
nodes <- df[keep_nodes] %>% .[order(song_id)] %>% .[, ..keep_cols]
setcolorder(nodes, c("song_id", setdiff(colnames(nodes), "song_id")))
fwrite(nodes, "./data/nodes70.csv")

g <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)

