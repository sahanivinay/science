# Load necessary libraries
library(cluster)
library(ggplot2)

# Numeric dataset
X <- c(2, 2, 8, 5, 7, 6, 1, 4, 3)
Y <- c(10, 5, 4, 8, 5, 4, 2, 9, 9)

# Combine the variables into a matrix
data_matrix <- cbind(X, Y)

# Perform hierarchical clustering
hc_result <- hclust(dist(data_matrix))

# Cut the dendrogram to create clusters (you can adjust the height as needed)
cut_height <- 4
clusters <- cutree(hc_result, h = cut_height)

# Display the cluster assignments
cat("Cluster Assignments:", clusters, "\n")

# Plot the dendrogram with cluster assignments
plot(hc_result, main = "Hierarchical Clustering", xlab = "Data Points", sub = paste("Cut Height =", cut_height))
rect.hclust(hc_result, k = length(unique(clusters)), border = 2:length(unique(clusters)))

# Performance metric - Silhouette Score
silhouette_score <- silhouette(clusters, dist(data_matrix))
cat("Silhouette Score:", mean(silhouette_score[, "sil_width"]), "\n")
