rm(list=ls())
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(corrplot)
####################Ejemplo con USAarrest
data("USArrests")
bd<-USArrests
names(bd)<-tolower(names(bd))
####################EIGEN
rho<-cor(bd)
corrplot(cor(bd))
eigen(rho)
## Comando R
mod1<-prcomp(bd, scale. = T, center = T)
summary(mod1)
plot(mod1)
corrplot(cor(mod1$x))
bd1<-bd %>% bind_cols(mod1$x)
round(cor(bd1),2)
biplot(mod1)
fviz_pca(mod1)
fviz_contrib(mod1, choice="var", axes = 1, top = 4)
fviz_contrib(mod1, choice="ind", axes = 1, top = 10)
## Comando R
mod2<-PCA(bd)
summary(mod2)
plot(mod2, c(2,3))
##########################
#Ejercicio: Evalué la factibilidad de usar PCA para resumir 
# las variables sobre discriminación en la EH24




