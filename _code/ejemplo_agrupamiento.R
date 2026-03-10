rm(list=ls())
library(cluster)
library(flexclust)
library(factoextra)
library(tidyverse)
library(Kmedians)
data("USArrests")
bd<-USArrests
#####################
#Kcenter
#####################
kcenter<-3
# media
set.seed(123)
m1 <- kmeans(bd, centers = kcenter)
# medoide
set.seed(123)
m2 <- pam(bd, k = kcenter, metric = "man")
# mediana
set.seed(123)
m3 <- Kmedians(bd,  kcenter)
#coeficiente de silueta
dd<-dist(bd)
m1c<-m1$cluster
m2c<-m2$clustering
m3c<-m3$bestresult$cluster
s1<-silhouette(m1c, dd)
s2<-silhouette(m2c, dd)
s3<-silhouette(m3c, dd)
plot(s1)
plot(s2)
plot(s3)
#opciones rapidas para media y medoide
fviz_nbclust(bd, kmeans, method = "silhouette")
fviz_nbclust(bd, kmeans, method = "wss")
fviz_nbclust(bd, pam, method = "silhouette")
fviz_nbclust(bd, pam, method = "wss")
####################################
#Jerárquico
####################################
dde<-dist(bd %>% scale(), method = "manhattan")
m1<-hclust(dde, method = "single")
m2<-hclust(dde, method = "complete")
m3<-hclust(dde, method = "average")
m4<-hclust(dde, method = "centroid")
m5<-hclust(dde, method = "ward.D")

plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(m5)

plot(m1, hang=-0.1)
plot(m2, hang=-0.1)
plot(m3, hang=-0.1)
plot(m4, hang=-0.1)
plot(m5, hang=-0.1)

k<-3
k_m1<-cutree(m1, k)
k_m2<-cutree(m2, k)
k_m3<-cutree(m3, k)
k_m4<-cutree(m4, k)
k_m5<-cutree(m5, k)

table(k_m1)
table(k_m2)
table(k_m3)
table(k_m4)
table(k_m5)

# ver los grupos de forma visual
plot(m1, hang=-0.1)
rect.hclust(m1, k=2)

plot(m2,hang=-0.1)
rect.hclust(m2,k=2)

plot(m3,hang=-0.1)
rect.hclust(m3,k=3)

plot(m4,hang=-0.1)
rect.hclust(m4,k=4)

plot(m5, hang=-0.1)
rect.hclust(m5,k=4)

fviz_dend(m1, k = 3,
          cex = 0.5, 
          rect = TRUE)
#elegir el valor de k
fviz_nbclust(bd, hcut, method = "silhouette", hc_method = "single")
fviz_nbclust(bd, hcut, method = "silhouette", hc_method = "complete")
fviz_nbclust(bd, hcut, method = "silhouette", hc_method = "average", hc_metric="euclidean")

#otras librerías
library(ape)
plot(as.phylo(m5), type="unrooted", cex=0.3)
