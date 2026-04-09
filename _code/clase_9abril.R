rm(list=ls())
library(tidyverse)
library(caret)
library(ExPanDaR)
library(tidymodels)
#Recolección de datos Y, X
data(iris)
bd<-iris
summary(iris)
clases<-unique(bd$Species)
bd<-bd %>% mutate(y=Species==clases[2])
###########
#Exploración y Preparación (IDA/EDA): Limpieza, manejo de valores perdidos y normalización (vital para algoritmos basados en distancia). Esta fase consume aproximadamente el 80% del esfuerzo del proyecto.
###########
glimpse(bd)
ExPanD(bd)
#Crear la base de datos de entrenamiento y prueba
set.seed(2238)
bd_split<-initial_split(
  bd %>% select(y, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width), prop = 0.8, strata = y)
bd_train<-training(bd_split)
bd_test<-testing(bd_split)
prop.table(table(bd_train$y))
prop.table(table(bd_test$y))
prop.table(table(bd$y))
#Entrenamiento del modelo: Selección del algoritmo adecuado basado en la naturaleza del problema.
m1<-glm(y~., data = bd_train, family = binomial(link="logit"))
yprob<-predict(m1, bd_test, type = "response")
t1<-table(yprob>0.5, bd_test$y)
t1
TP<-t1[2,2] ; TN<-t1[1,1]; FN<-t1[1,2]; FP<-t1[2,1]
ss<-TP/(TP+FN)
es<-TN/(TN+FP)
#Evaluación del desempeño: Uso de métricas para verificar si el algoritmo aprendió patrones o solo memorizó los datos.
aux<-data.frame()
for(i in seq(0,1,0.01)){
  yp<-factor(yprob>i, levels = c(F,T))
  yr<-factor(bd_test$y, levels = c(F,T))
  t1<-table(yp, yr)
  print(t1)
  TP<-t1[2,2] ; TN<-t1[1,1]; FN<-t1[1,2]; FP<-t1[2,1]
  ss<-TP/(TP+FN)
  es<-TN/(TN+FP)
  aux<-aux %>% bind_rows(data.frame(i, ss,es))
}
plot(1-aux$es, aux$ss,  xlim=c(0,1), ylim=c(0,1), type = "l")
abline(a=0,b=1, lty=2)
plot(pROC::roc(bd_test$y, yprob))
#Mejora del modelo: Ajuste de parámetros o uso de métodos de ensamble para maximizar la precisión.
