# Load necessary libraries
library(cluster)
library(ggplot2)

# Numeric dataset
X <- c(2, 2, 8, 5, 7, 6, 1, 4, 3)
Y <- c(10, 5, 4, 8, 5, 4, 2, 9, 9)

# Combine the variables into a matrix
data_matrix <- cbind(X, Y)

# Perform k-means clustering with k = 2 (you can adjust k as needed)
k <- 2
kmeans_result <- kmeans(data_matrix, centers = k)

# Display the cluster assignments
cluster_assignments <- kmeans_result$cluster
cat("Cluster Assignments:", cluster_assignments, "\n")

# Display the cluster centers
cluster_centers <- kmeans_result$centers
cat("Cluster Centers:\n", cluster_centers, "\n")

# Plot the data points with cluster assignments
plot(X, Y, col = cluster_assignments, pch = 19, main = "K-Means Clustering", xlab = "X", ylab = "Y")
points(cluster_centers[, 1], cluster_centers[, 2], col = 1:k, pch = 8, cex = 2)
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 8, cex = 1.5, title = "Clusters")

# Performance metric - Silhouette Score
silhouette_score <- silhouette(cluster_assignments, dist(data_matrix))
cat("Silhouette Score:", mean(silhouette_score[, "sil_width"]), "\n")

