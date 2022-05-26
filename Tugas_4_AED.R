library(MASS)
library(dplyr)
library(ggplot2)
library(rcompanion)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(FactoMineR)
library(caret)

data_Life_Expect <- as.data.frame(read.csv("Life Expectancy Data.csv"))

# Handling Missing Value
kolom_NA <- colSums(is.na(data_Life_Expect)) / dim(data_Life_Expect)[1] * 100
kolom_NA_delete <- names(which(kolom_NA >= 10))
kolom_NA_imputation <- names(which((kolom_NA > 0) & (kolom_NA <  10)))


## delete Many column because missing value > 10%
data_Life_Expect <- data_Life_Expect %>% dplyr::select(-c(kolom_NA_delete))
kolom_NA_numeric <- names(which(lapply(data_Life_Expect %>% dplyr::select(kolom_NA_imputation) ,class) == "numeric" ))
kolom_NA_integer <- names(which(lapply(data_Life_Expect %>% dplyr::select(kolom_NA_imputation) ,class) == "integer" ))

## mean Imputation
for(j in kolom_NA_numeric){
  data_Life_Expect[[j]][is.na(data_Life_Expect[[j]])] <- mean(data_Life_Expect[[j]],na.rm=T)
}

for(j in kolom_NA_integer){
  data_Life_Expect[[j]][is.na(data_Life_Expect[[j]])] <- as.integer(mean(data_Life_Expect[[j]],na.rm=T))
}

data_Life_Expect$Status <- factor(data_Life_Expect$Status,levels=c("Developing","Developed"))
data_Life_Expect <- data_Life_Expect %>% dplyr::select(-Country)
data_Life_Expect$Status <- as.numeric(data_Life_Expect$Status) - 1
data_Life_Expect_X <- data_Life_Expect %>% dplyr::select(-Life.expectancy)
data_Life_Expect_Y <- data_Life_Expect %>% dplyr::select(Life.expectancy)


# rPartMod <- train(Life.expectancy ~ ., data=data_Life_Expect, method="rpart")
# rpartImp <- varImp(rPartMod)
# print(rpartImp)
# tabel_imp <- rpartImp$importance
# tabel_imp <- tabel_imp %>% filter(Overall > 0)
# col_selected <- rownames(tabel_imp)
# data_Life_Expect_X <- data_Life_Expect %>% dplyr::select(col_selected)


# library(FSelector)
# hasil <- information.gain(Life.expectancy~., data_Life_Expect)
# # 0.5
# hasil <- hasil %>% filter(attr_importance > 0.5) %>% arrange(attr_importance)
# data_Life_Expect_X <- data_Life_Expect_X %>% dplyr::select(rownames(hasil))



data_Life_Expect_X_std <- as.data.frame(scale(data_Life_Expect_X))
apply(data_Life_Expect_X_std,2,mean)
apply(data_Life_Expect_X_std,2,sd)



# Elbow method for Searching the best Clustering 
fviz_nbclust(data_Life_Expect_X_std, kmeans, method = "wss")
fviz_nbclust(data_Life_Expect_X_std, kmeans, method = 'silhouette')


# inference, the best k is 2

set.seed(123)
km <- kmeans(x = data_Life_Expect_X_std, centers = 2,
             nstart = 5, iter.max = 100)
fviz_cluster(object = km,
             data = data_Life_Expect_X, palette = "jco",
             ggtheme = theme_minimal(),geom="point")

table(km$cluster) #banyak objek setiap cluster
km$centers #rata2 peubah setiap cluster



library(FactoMineR)
km <- kmeans(x = data_Life_Expect_X_std, centers = 3,
             nstart = 5, iter.max = 100)
centroid <- km$centers
res.pca <- PCA(centroid,  graph = FALSE)
fviz_pca_biplot(res.pca, repel = T)

res.hc <- hclust(dist(data_Life_Expect_X), method = "ward.D2")
fviz_dend(x = res.hc, cex = 0.5, k = 6, palette = "jco") 
###heatmaps
library(pheatmap)
pheatmap(t(data_Life_Expect_X), cutree_cols = 3, 
         fontsize_col = 8.5,
         legend = T)
