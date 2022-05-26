# CLUSTERING #
library(factoextra)
library(datasets)
dataset(midwest)
data<-midwest
county<-data$county[1:102]
data.obs<-data[data$state=="IL",]
data.obs<-data.obs[,c(18:20,23)]
rownames(data.obs)<-county
rownames(data.obs)


#------NON-HIERARCHICAL---------
#------K-MEANS CLUSTERING-------
set.seed(123)
km <- kmeans(x = data.obs, centers = 3, nstart = 5)
fviz_cluster(object = km, 
             data = data.obs, palette = "jco", 
             ggtheme = theme_minimal())
table(km$cluster) #banyak objek setiap cluster
km$centers #rata2 peubah setiap cluster


#visualisasi b-plot
library(FactoMineR)
centroid <- km$centers
rownames(centroid) <- c("CL1", "CL2", "CL3")

dim(centroid)
centroid

res.pca <- PCA(centroid,  graph = FALSE)
fviz_pca_biplot(res.pca, repel = T)



#------------HIERARCHICAL-----------------
res.hc <- hclust(dist(data.obs), method = "ward.D2")
fviz_dend(x = res.hc, cex = 0.5, k = 4, palette = "jco") 
###heatmaps
library(pheatmap)
pheatmap(t(data.obs), cutree_cols = 3, 
         fontsize_col = 8.5,
         legend = T)
