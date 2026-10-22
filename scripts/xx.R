# Dummy RSCU dataset (20 plastomes × 10 codons)

set.seed(123)

rscu_matrix <- data.frame(
  Plastome = paste0("P", 1:20),
  
  UUU = runif(20, 0.5, 2.0),
  UUC = runif(20, 0.3, 1.5),
  UUA = runif(20, 0.8, 2.2),
  UUG = runif(20, 0.2, 1.2),
  CUU = runif(20, 0.4, 1.8),
  CUC = runif(20, 0.3, 1.4),
  CUA = runif(20, 0.7, 2.1),
  CUG = runif(20, 0.2, 1.0),
  AUU = runif(20, 0.6, 2.0),
  AUC = runif(20, 0.4, 1.6)
)

# View data
print(rscu_matrix)

rownames(rscu_matrix) <- rscu_matrix$Plastome
rscu_matrix <- rscu_matrix[, -1]

pca <- prcomp(rscu_matrix, scale. = TRUE)

pca_df <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  Plastome = rownames(pca$x)
)

library(ggplot2)

ggplot(pca_df, aes(PC1, PC2, label = Plastome)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  theme_classic() +
  ggtitle("PCA of Dummy RSCU Data")

install.packages("uwot")
library(uwot)

umap_result <- umap(rscu_matrix,
                    n_neighbors = 5,
                    min_dist = 0.3,
                    metric = "euclidean")

umap_df <- data.frame(
  UMAP1 = umap_result[,1],
  UMAP2 = umap_result[,2],
  Plastome = rownames(rscu_matrix)
)

ggplot(umap_df, aes(UMAP1, UMAP2, label = Plastome)) +
  geom_point(size = 3, color = "blue") +
  geom_text(vjust = -0.5) +
  theme_classic() +
  ggtitle("UMAP of Dummy RSCU Data")
# Create fake biological groups (e.g., species clades)
group <- rep(c("Group_A", "Group_B", "Group_C", "Group_D"), each = 5)

rscu_matrix$Group <- group

library(uwot)

umap_input <- rscu_matrix[, !(colnames(rscu_matrix) %in% c("Group"))]

umap_result <- umap(umap_input,
                    n_neighbors = 5,
                    min_dist = 0.3,
                    metric = "euclidean")
library(ggplot2)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Group)) +
  geom_point(size = 3, alpha = 0.9) +
  theme_minimal() +
  ggtitle("UMAP of RSCU Data (Colored by Group)")
