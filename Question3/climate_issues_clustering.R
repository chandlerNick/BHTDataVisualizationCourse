# Nick Chandler
# 27.04.2025
# Clustering of current climate issues

library(reticulate)
library(text)
library(plotly)
library(ggplot2)
library(gridExtra)
library(cluster)
library(factoextra)

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
embedding_file <- "../data/climate_issues_embeddings_cached.rds"
if (file.exists(embedding_file)) {
  message("Loading cached embeddings...")
  embeddings <- readRDS(embedding_file)
} else {
  message("Computing embeddings...")
  current_climate_issues <- trim_df$Environment..Environment...current.issues
  embeddings <- textEmbed(current_climate_issues)
  saveRDS(embeddings, embedding_file)
}
embeddings <- embeddings$texts$texts  # Matrix (n_country, embed_dim)
dim(embeddings)

reticulate::py_config()
reticulate::py_run_string("import nltk; print(nltk.data.path)")

### K-means
set.seed(42)

# Elbow plot
max.k <- 10
wss <- sapply(1:max.k, function(k) {
  kmeans_result <- kmeans(embeddings, centers=k, nstart=25)
  return(kmeans_result$tot.withinss)
}) 


elbow_plot <- data.frame(k=1:max.k, WSS=wss)

elbow_plot_gg <- ggplot(elbow_plot, aes(x=k, y=WSS)) +
  geom_line() +
  geom_point() +
  labs(title="Elbow Plot for K-means Clustering",
       x="Number of Clusters (k)",
       y="Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

optimal_k <- 4  # Set this!

kmeans_result_final <- kmeans(embeddings, centers=optimal_k, nstart=25)

table(kmeans_result_final$cluster)


# PCA
pca_final <- prcomp(embeddings, scale.=TRUE)
pca_final$sdev

pca_var <- pca_final$sdev^2
pve <- pca_var / sum(pca_var)
cpve <- cumsum(pve)

# Create dataframe
pca_df <- data.frame(
  PC = factor(1:length(pve)),
  var = pca_var,
  PVE = pve,
  CPVE = cpve
)

# Scree plot
scree_plot <- ggplot(pca_df, aes(x = PC, y = var)) +
  geom_col(fill="steelblue") +
  geom_point(size=2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="Scree Plot",
       x = "Principal Component",
       y = "Variance") +
  theme_minimal()

# CPVE plot
cpve_plot <- ggplot(pca_df, aes(x = as.integer(as.character(PC)), y = CPVE)) +
  geom_line(size=1, color="darkgreen") +
  geom_point(size=2, color = "darkgreen") +
  scale_x_continuous(breaks=1:nrow(pca_df)) +
  labs(title="Cumlative Proportion of Variance Explained",
       x = "Number of Principal Components",
       y = "Cumulative PVE") +
  theme_minimal()


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

# Prepare data
pca_data_3d <- data.frame(pca_final$x[,1:3], cluster=as.factor(kmeans_result_final$cluster))
colnames(pca_data_3d)[1:3] <- c("PC1", "PC2", "PC3")

# Plot 1: With clusters
plot_3d_clusters <- plot_ly(pca_data_3d,
                            x = ~PC1, y= ~PC2, z= ~PC3,
                            color=~cluster, colors= "Set1",
                            type="scatter3d", mode="markers",
                            marker=list(size=4)) %>%
  layout(title="3D PCA of Text Embeddings eith K-means Clusters")

# Plot 2: Without clusters
plot_3d_nocolor <- plot_ly(pca_data_3d,
                           x = ~PC1, y = ~PC2, z = ~PC3,
                           type = "scatter3d", mode = "markers",
                           marker = list(size=4, color="steelblue")) %>%
  layout(title="3D PCA of Text Embeddings with K-Means Clusters")


# Silhouette plots of the cluster GOF
sil <- silhouette(kmeans_result_final$cluster, dist(scale(embeddings)))
sil_plot <- fviz_silhouette(sil)


# Save plots
ggsave("climate_issues_clustering_elbow_plot.pdf", plot = elbow_plot_gg, width = 6, height = 4)
ggsave("climate_issues_clustering_pca_with_clusters_2PCs.pdf", plot = plot_with_clusters, width = 6, height = 4)
ggsave("climate_issues_clustering_pca_no_clusters_2PCs.pdf", plot = plot_without_clusters, width = 6, height = 4)
ggsave("climate_issues_clustering_silhouette_plot.pdf", plot = sil_plot, width=6, height=4)

# View plots
plot_3d_clusters
plot_3d_nocolor
elbow_plot_gg
plot_with_clusters
plot_without_clusters
sil_plot
scree_plot
cpve_plot








