# PCA (Principal Component Analysis)
# function: prcomp()

# create data
data.matrix <- matrix(nrow = 100, ncol = 10)
colnames(data.matrix) <- c(
  paste("wt", 1:5, sep = ""), 
  paste("ko", 1:5, sep = ""))
rownames(data.matrix) <- paste("gene", 1:100, sep = "") 
for (i in 1:100) {
  wt.values <- rpois(5, lambda = sample(x=10:1000, size = 1))
  ko.values <- rpois(5, lambda = sample(x=10:1000, size = 1))
  data.matrix[i, ] <- c(wt.values, ko.values)
}

head(data.matrix)

# PCA: prcomp()
# the example data created variables in row and cases in column, therefore the data matrix should be transposed using t() function. 
pca <- prcomp(t(data.matrix), scale = TRUE) # prcomp() retruns x, sdev, and rotaion

# PCA:principal compoents = pca$x[, component#]
# number of the all principal components = number of variables, 10
# x contains the principal components for drawing a graph. 
# x columns are in the order of importace of the principal components
# ploting data by the two most important principal components
plot(pca$x[, 1], pca$x[, 2])

# PCA: importance of the principal components using the varinace of the projected values of the data to each principal line, pca$sdev
pca.var <- pca$sdev^2 # variance of each principal component (projected to each principal line)
pca.var.percentage <- round(pca.var / sum(pca.var)*100, 1) # variance percentage of each principal compoent
barplot(pca.var.percentage, main = "Scree Plot", xlab = "Principal Component", ylab = "Percent Variation")

# make fancy plot using ggplot2
library(ggplot2)
pca.data <- data.frame(Sample = rownames(pca$x), x = pca$x[,1], y=pca$x[,2])
head(pca.data)
ggplot(data = pca.data, aes(x=x, y =y, label = Sample)) +
  geom_text() +  # plot labels rather than dots or shapes
  xlab(paste("PC1 - ", pca.var.percentage[1], "%", ssep = "")) +
  ylab(paste("PC2 - ", pca.var.percentage[2], "%", ssep = "")) +
  theme_bw() +  # makes the graph's background white
  ggtitle("My PCA Graph")

# PCA: loadning scores for each PC = pca$rotation[, 1]
# PC scores of each data = loading_score of var1 * data of var1 + loading_score of var2 * data of var2 + ...
# the loading scores are coefficient of the linear line for a PC
loading_scores <- pca$rotation[, 1] # loading scores of the first component
head(loading_scores)

gene_scores <- abs(loading_scores) # magnitude of the loading scores (+ or - coefficient of the PC line)
gene_score_ranking <- sort(gene_scores, decreasing = TRUE)
top_10_genes <- names(gene_score_ranking[1:10])
top_10_genes
pca$rotation[top_10_genes,1]
