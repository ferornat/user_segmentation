rm(list=ls())
gc()
.rs.restartR()
setwd("")

# Libreries
library("xlsx")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("bigrquery")
library("car")
library("corrplot")
library("DT") # Para hacer Tabla
library("readxl")
library("factoextra")
# https://cran.r-project.org/web/packages/FeatureImpCluster/readme/README.html
library("FeatureImpCluster")
library("flexclust")
library("clustMixType")
library('sqldf')
library("ktaucenters")

# We upload the file
df <- read.csv("~/file.csv", encoding="UTF8")
df <- df[complete.cases(df),]

# We get the month
df <- df[which(df$month_order_date == '2021-11-01'),]


# Unifying bases


df1 <- sqldf("SELECT a.*,b.var
                FROM df a
                INNER JOIN df2 b USING(id_var)")

rm(df)

# We look for correlations
library("corrplot")

# Correlas
M <- cor(sel_var)
corrplot(M, method = 'number', order = "AOE") 
corrplot(M, order = 'AOE')

rm(sel_var,M)

# We evaluate the clusters
# Elbow Method

set.seed(123)
clusters.sin_ids <- df
nums <- sapply(clusters.sin_ids, is.numeric)
clusters.sin_ids.numerics <- clusters.sin_ids[, nums]

# Graficamos:
n.clusters <- 10
wss <- (nrow(clusters.sin_ids.numerics)-1)*sum(apply(clusters.sin_ids.numerics,2,var))
for (i in 2:n.clusters){
  wss[i] <- sum(kmeans(clusters.sin_ids.numerics,centers=i)$withinss)
}

plot(1:n.clusters, wss, type="b", xlab="Number of Clusters",
     ylab="WCV",
     main="#Clusters",
     pch=20, cex=2)

n.clusters <- 10
wss <- (nrow(clusters.sin_ids.numerics)-1)*sum(apply(clusters.sin_ids.numerics,2,var))
for (i in 2:n.clusters){
  wss[i] <- sum(kmeans(clusters.sin_ids.numerics,centers=i)$betweenss)
}
plot(1:n.clusters, wss, type="b", xlab="Number of clusters",
     ylab="BCV",
     main="#Clusters",
     pch=20, cex=2)


rm(nums,clusters.sin_ids,i,n.clusters,wss)

# We check Principal Components
pca_1 <- prcomp(clusters.sin_ids.numerics,
                center = TRUE,
                scale. = TRUE)

print(pca_1)
summary(pca_1)
summary(pca_1)$sd^2
plot(pca_1, type = "l") # Acá consideramos 2 clusters

# We make a plot:
fviz_eig(pca_1)
fviz_pca_var(pca_1,
             col.var = "contrib", # Color por contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Evitamos el overlapping
)

rm(pca_1)

### Applying the classic algortihm  ###
set.seed(123)
kmeans_output <- kmeans(scale(df[,-1]),centers = 5,nstart = 10) # Always scale

kmeans_output$centers
kmeans_output$cluster

df$cluster_k_means <- factor(kmeans_output$cluster)


rm(kmeans_output)

### Checking Feature Importance ###

set.seed(10)
res <- kcca(df[,-c(8,13,15,16)],k = 3) # We avoid using the categorical columns
set.seed(10)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(df[,-c(8,13,15,16)]))
plot(FeatureImp_res)
barplot(res)

### Applying the robust algortihm  ####
### Robust K-means Version ###
### Ktaucenters library ###

set.seed(123)
kmeans_output2 <- ktaucenters(scale(clusters.sin_ids.numerics),K = 3,nstart = 10)

df$cluster_rob_3 <- factor(kmeans_output2$cluster)

# Looking at results

agrup <- df %>% group_by(cluster_rob_3) 
summary_table <- summarise (agrup,
                            col1 = mean(var1, na.rm = TRUE),
                            col2 = mean(var2, na.rm = TRUE),
                            col3 = mean(var3, na.rm = TRUE),
                            n_rows = n())

# We keep the results
write_csv(cruce2,'clusters_arg.csv')
