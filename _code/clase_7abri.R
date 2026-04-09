rm(list=ls())
library(tidyverse)
library(tidymodels)
#install.packages("tidymodels")
library(caret)
library(ExPanDaR)
#install.packages("caret")
#Recolección de datos Y, X
data(iris)
bd<-iris
summary(iris)
clases<-unique(bd$Species)
bd<-bd %>% mutate(y=Species==clases[1])
###########
#Exploración y Preparación (IDA/EDA): Limpieza, manejo de valores perdidos y normalización (vital para algoritmos basados en distancia). Esta fase consume aproximadamente el 80% del esfuerzo del proyecto.
###########
#Exploración (formato, distribución)
str(bd)
boxplot(bd$Sepal.Length)
boxplot(bd$Sepal.Width)
boxplot(bd$Petal.Width)
boxplot(bd$Petal.Length)
#tabla
bd %>% group_by(y) %>% 
  summarise(x1=mean(Sepal.Length),
            x2=mean(Sepal.Width),
            x3=mean(Petal.Length),
            x4=mean(Petal.Width),)
bd %>% group_by(y) %>% 
  summarise_at(
    vars(Sepal.Length:Petal.Width), 
    sd
    )
#test estadístico
t.test(Sepal.Length~y , data=bd)
t.test(Petal.Length~y , data=bd)
t.test(Sepal.Width~y , data=bd)
t.test(Petal.Width~y , data=bd)
#Visual
boxplot(Sepal.Length~y , data=bd)
boxplot(Petal.Length~y , data=bd)
boxplot(Sepal.Width~y , data=bd)
boxplot(Petal.Width~y , data=bd)
ggplot(bd, aes(Sepal.Length,col=y))+
  geom_density()
ggplot(bd, aes(Petal.Length,col=y))+
  geom_density()
#Transformación (mutate)

#Crear la base de datos de entrenamiento y prueba
set.seed(842)
bd<-bd %>% mutate(aux=runif(150)) %>% arrange(aux)
bd_train<-bd[1:105 ,]
bd_test<-bd[106:150 ,]
prop.table(table(bd_train$y))
prop.table(table(bd_test$y))
prop.table(table(bd$y))

#Entrenamiento del modelo: Selección del algoritmo adecuado basado en la naturaleza del problema.
#Evaluación del desempeño: Uso de métricas para verificar si el algoritmo aprendió patrones o solo memorizó los datos.
#Mejora del modelo: Ajuste de parámetros o uso de métodos de ensamble para maximizar la precisión.