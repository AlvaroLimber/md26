rm(list=ls())
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(explor)
library(haven)
#################################
bd<-read_sav("_dataset/EH2024_Discriminacion.sav")
bd<-bd %>% mutate(disc1=s09a_01a==1,
              disc2=s09a_01b==1,
              disc3=s09a_01c==1,
              disc4=s09a_01d==1,
              disc5=s09a_01e==1,
              disc6=s09a_01f==1,
              disc7=s09a_01g==1,
              disc8=s09a_01h==1,
              disc9=s09a_01i==1,
              disc10=s09a_01j==1,
              disc11=s09a_01k==1,
              disc12=s09a_01l==1)
corrplot(cor(bd %>% select(disc1:disc12)))
#Correlación
mod1<-bd %>% select(disc1:disc12) %>% prcomp(scale. = T, center = T)
summary(mod1)
plot(mod1)
biplot(mod1)
fviz_pca_biplot(mod1)
bd[91, 46:56] %>% View()
#covarianza
mod2<-bd %>% select(disc1:disc12) %>% prcomp(scale. = F, center = F)
summary(mod2)
plot(mod2)
biplot(mod2)
fviz_pca_biplot(mod2)
#Dashboard
explor(mod1)
