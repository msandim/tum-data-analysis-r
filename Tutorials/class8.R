library(mclust)
library(pheatmap)
library(data.table)
library(magrittr)
library(ggplot2)

# 1) PCA questions
iris2 <- as.data.table(iris)
iris2[, Species:=NULL]

# 1.1
ir.pca <- prcomp(iris2, center = TRUE, scale. = TRUE) 
summary(ir.pca)
# 0.7296 0.2285 0.03669 0.00518

# 1.2
iris_trans <- scale(iris2) %*% ir.pca$rotation
all(predict(ir.pca) == iris_trans)

qplot(PC1, PC2, geom="point", data=data.frame(iris_trans, Species = iris$Species),
      color = Species)

# 1.3 eu xei laaa

# 2.1
plot.data <- iris[, -5]
rownames(plot.data) <- paste("iris", 1:nrow(plot.data), sep=".")
row.ann <- data.frame(species = iris[, 5])
rownames(row.ann) <- rownames(plot.data)
pheatmap(plot.data,
         clustering_method = "complete",
         annotation_row = row.ann,
         show_rownames = FALSE)

# 2.2
plot.data <- iris[, -5]
rownames(plot.data) <- paste("iris", 1:nrow(plot.data), sep=".")
row.ann <- data.frame(species = iris[, 5])
rownames(row.ann) <- rownames(plot.data)
h1 <- pheatmap(plot.data,
         clustering_method = "complete",
         annotation_row = row.ann,
         show_rownames = FALSE)

complete <- cutree(h1$tree_row, k=3)


# 2.3
plot.data <- iris[, -5]
rownames(plot.data) <- paste("iris", 1:nrow(plot.data), sep=".")
row.ann <- data.frame(species = iris[, 5], complete = factor(complete))
rownames(row.ann) <- rownames(plot.data)
h2 <- pheatmap(plot.data,
         clustering_method = "average",
         annotation_row = row.ann,
         show_rownames = FALSE)

average <- cutree(h2$tree_row, k=3)
table(complete, average)

# 3
clust_km <- kmeans(iris[, -5], 3)

plot.data <- iris[, -5]
rownames(plot.data) <- paste("iris", 1:nrow(plot.data), sep=".")
row.ann <- data.frame(species = iris[, 5], kmeans = factor(clust_km$cluster), complete = factor(complete), average = factor(average))
rownames(row.ann) <- rownames(plot.data)
h2 <- pheatmap(plot.data,
               clustering_method = "average",
               annotation_row = row.ann,
               show_rownames = FALSE)

# well, kmeans gives results very similar to the average, but worse

# 4.1
clu_fit <- Mclust(iris[, c("Petal.Length", "Petal.Width")], G=3)

# 4.2
pe_len <- seq(min(iris$Petal.Length), max(iris$Petal.Length), 0.01)
pe_wid <- seq(min(iris$Petal.Width), max(iris$Petal.Width), 0.01)

# cluster 3 is bigger because of the covariance, we basically have a very big yellow elipse 

# 5.1
Rand_index <- function(X, Y)
{
  pairs <- t(combn(1:length(X), 2))
  
  
}

# mixtures models are the best

