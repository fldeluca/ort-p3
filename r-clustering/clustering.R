#Datasets
dataset <- read.csv('~/ort-p3/datasets/AggregationClusters2.csv')


ggplot() + 
  geom_point(aes(x = X, y = Y), data = dataset, alpha = 0.5) + 
  ggtitle('Conjunto de Datos')

#K-means Clustering
set.seed(1234)
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(dataset, i)$withinss)
}

ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') +
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') +
  ggtitle("Método del Codo") +
  xlab('Cantidad de Centroides k') +
  ylab('WCSS')

kmeans <- kmeans(dataset, 7, iter.max = 1000, nstart = 10)

dataset$cluster <- kmeans$cluster

ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = kmeans$centers[, 1], y = kmeans$centers[, 2]), color = 'black', size = 3) +
  ggtitle('Clusters de Datos con k = 7 / K-Medios') +
  xlab('X') + ylab('Y')

#Hierarchical Clustering
dendrogram <- hclust(dist(dataset, method = 'euclidean'), method = 'ward.D')
ggdendrogram(dendrogram, rotate = FALSE, labels = FALSE, theme_dendro = TRUE) +
  labs(title = "Dendrograma")

agrupamientoJ <- hclust(dist(dataset, method = 'euclidean'), method = 'ward.D')
clases_aj <- cutree(agrupamientoJ, k = 3)
dataset$cluster <- clases_aj

ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  ggtitle('Clusters de Datos con k = 3 / Agrupamiento Jerárquico') +
  xlab('X') + ylab('Y')






