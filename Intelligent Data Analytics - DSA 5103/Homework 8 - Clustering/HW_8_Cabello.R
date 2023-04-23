library(ggplot2)
library(cluster)  
library(useful)     
library(NbClust)    
library(rgl)
library(factoextra)
library(fpc)
library(dbscan)
library(pastecs)

heartData = read.table("processed.cleveland.data",  fileEncoding = "UTF-8", sep = ",", header = TRUE, col.names = c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'thalach', 'exang', 'oldpeak','slope', 'ca', 'thal', 'num'))

heartData$ca = as.numeric(as.factor(heartData$ca))
heartData$thal = as.numeric(as.factor(heartData$thal))

#Scaling data
heartScaled = scale(heartData)
heartScaled = data.frame(heartScaled)

# Descriptive Stats based on healthy or not healthy records
healthy = subset(heartScaled, heartData$num == 0)
notHealthy = subset(heartScaled, heartData$num != 0)

summary(healthy)
summary(notHealthy)

# K-MEANS CLUSTERING - K Selection

# METHOD 1 - WSSPLOT
wssplot  =  function(data, nc=15){                    
  
  par(mfrow=c(1,2))
  wss  =  NULL  
  pctExp  = NULL
  
  for (k in 1:nc)
  {
    kclus  =  kmeans(data, centers=k)
    wss[k]  =  kclus$tot.withinss      
    pctExp[k]  =  1-wss[k]/kclus$totss
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", lty = 0.5)
  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained", lty = 0.5)
  par(mfrow=c(1,1))
}

wssplot(heartScaled[1:13],nc=30)

# METHOD 2 - Hartigans rule
hart_rule = FitKMeans(heartScaled[1:13],max.clusters=30,nstart=20)
hart_rule
PlotHartigan(hart_rule)

# METHOD 3 - nbclust
NbClust(heartScaled[1:13],method="kmeans")

# K-Means Application - K=5
kmeans_results = kmeans(heartScaled[1:13],5)
heartScaled$cluster = as.factor(kmeans_results$cluster)
table(heartScaled$num, heartScaled$cluster)
fviz_cluster(kmeans_results, data = heartScaled[1:13], geom = "point")

#Plotting results - PCA
plot(kmeans_results,data=heartScaled[1:13])

#Centroid info
clusInfo = data.frame(kmeans_results$centers,kmeans_results$size)
clusInfo

# K-Means Application - K=6
kmeans_results = kmeans(heartScaled[1:13],6)
heartScaled$cluster = as.factor(kmeans_results$cluster)
table(heartScaled$num, heartScaled$cluster)
fviz_cluster(kmeans_results, data = heartScaled[1:13], geom = "point")

# Hierarchical Clustering - Euclidean dist
#Calc distances
dist_euc = dist(heartScaled[1:13], method="euclidean")

hier_clust = hclust(dist_euc, method="ward.D")

#plotting results
plot(hier_clust)

#highlighting the 5 classes
rect.hclust(hier_clust, k=5, border="red")

heartScaled$hcluster=as.factor(cutree(hier_clust, k=5))   
table(heartScaled$num, heartScaled$hcluster)

# DBSCAN
#turning df to matrix
heartMatrix = data.matrix(heartScaled[1:13])

distplot_results = kNNdistplot(heartMatrix, k=5)

set.seed(10)
dbscan_results = dbscan(heartScaled[1:13], eps = 5, minPts = 250)

table(dbscan_results$cluster, heartScaled$num)

#Plotting results
fviz_cluster(dbscan_results, heartScaled[1:13], geom = "point")
