# Nick Chandler
# 27.04.2025
# Clustering of current climate issues

library(reticulate)
library(text)
library(ggplot2)
library(gridExtra)

# Set up python dependencies for text
use_python("C:/Users/njcha/OneDrive/Documents/.virtualenvs/r-reticulate/Scripts/python.exe", required = TRUE)

col_names_and_nas <- function(df) {
  for (i in seq_along(df)) {
    cat(colnames(df)[i], ":", sum(is.na(df[[i]])), "\n")
  }
}

### Read in data
full_df <- read.csv("../data/clean_data.csv")
col_names_and_nas(full_df)  # Diagnostics

# Trim data
sub_cols <- grep("^Environment..Environment", colnames(full_df), value=TRUE)

trim_df <- full_df[,sub_cols]
trim_df$country <- full_df[,1]
rownames(trim_df) <- full_df[,1]

# Basic info
col_names_and_nas(trim_df)
head(trim_df)


### Embed all text
current_climate_issues <- trim_df$Environment..Environment...current.issues
embeddings <- textEmbed(current_climate_issues)

dim(embeddings)

### Error with the embeddings
print(embeddings)

reticulate::py_config()
reticulate::py_run_string("import nltk; print(nltk.data.path)")

### K-means
set.seed(42)

# Elbow plot
wss <- sapply(1:10, function(k) {
  kmeans_result <- kmeans(embeddings, centers=k, nstart=25)
  return(kmeans_result$tot.withinss)
}) 


elbow_plot <- data.frame(k=1:15, WSS=wss)

ggplot(elbow_plot, aes(x=k, y=WSS)) +
  geom_line() +
  geom_point() +
  labs(title="Elbow Plot for K-means Clustering",
       x="Number of Clusters (k)",
       y="Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

optimal_k <- 7  # Set this!

kmeans_result_final <- kmeans(embeddings, centers=optimal_k, nstart=25)

table(kmeans_result_final$cluster)


# PCA
pca_final <- prcomp(embeddings, scale.=TRUE)

### Plots

# First two PCs
pca_data <- data.frame(pca_final$x[, 1:2], cluster=as.factor(kmeans_result_final$cluster))

# Plot 1: PCA with clusters
plot_with_clusters <- ggplot(pca_data, aes(x=PC1, y=PC2, color=cluster)) +
  geom_point(size=3) +
  labs(title="PCA of Text Embeddings with K-means Clusters") +
  theme_minimal()

# Plot 2: PCA without clusters
pca_data_no_cluster <- data.frame(pca_final$x[, 1:2])
plot_without_clusters <- ggplot(pca_data_no_cluster, aes(x=PC1, y=PC2)) +
  geom_point(size=3, color="steelblue") +
  labs(title="PCA of Text Embeddings (No Clusters)")

# Combine side-by-side
grid.arrange(plot_with_clusters, plot_without_clusters, ncol=2)


