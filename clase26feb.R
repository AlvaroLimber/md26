rm(list=ls())
library(tidyverse)
library(corrplot)
#####################
data(USArrests)
bd<-USArrests
#####################
plot(bd)
rho<-cor(bd)
det(rho)
sigma<-var(bd)
det(sigma)
corrplot(rho)
####################
eigen(sigma)
aux<-eigen(rho)
sum(aux$values)
L<-aux$vectors
round( L %*% t(L), 2)

L%*% diag(aux$values) %*% t(L)
rho

L[,1:3]%*%
  diag(aux$values)[1:3,1:3]%*%
  t(L[,1:3])

L[,1:2]%*%
  diag(aux$values)[1:2,1:2]%*%
  t(L[,1:2])

prcomp()
pca<-prcomp(bd, center = T, scale. = T)
pca$x




