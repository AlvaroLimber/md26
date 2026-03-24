#Ejemplos de regresión según los pasos
#0. Pregunta de investigación, revisión de literatura 
#¿La educación mejora los ingresos de las personas de 25 años o más?
#1. Base datos lista para el modelo (Unidad de investigación)
#2. Establecer la relación interés
#3. Definir el modelo de interés
#4. Optimizar el modelo
#5. Validar el modelo
#6. Analizar/Predecir a partir del modelo
rm(list=ls())
library(tidyverse)
library(haven)
library(labelled)
library(ggplot2)
library(GGally)
library(quantreg)
library(car)
###########################################
load("_dataset/eh24.RData")
bd<-eh24p %>% mutate(edad=as.numeric(s01a_03),
                     female=s01a_02==2) %>% filter(edad>=25)
m1<-rq(ylab~aestudio+female, data= bd, tau=0.5)
m2<-rq(ylab~aestudio+female, data= bd, tau=0.5, method = "fn")
deciles<-seq(0.1,0.9,0.1)
m3<-rq(log(ylab)~aestudio+female, data= bd, tau=deciles, method = "fn")
summary(m2)
summary(m3)
print(anova(m3, joint = F))
AIC(m3)
AIC(m2)
plot(m3)
#VIF sobre lm