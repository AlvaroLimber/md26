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
#1. Base datos lista para el modelo (Unidad de investigación)
#dataset eh2024
eh24d<-read_sav("/home/alvaro/Downloads/BD_EH2024/EH2024_Discriminacion.sav")
eh24e<-read_sav("/home/alvaro/Downloads/BD_EH2024/EH2024_Equipamiento.sav")
eh24g<-read_sav("/home/alvaro/Downloads/BD_EH2024/EH2024_Gastos_Alimentarios.sav")
eh24p<-read_sav("/home/alvaro/Downloads/BD_EH2024/EH2024_Persona.sav")
eh24s<-read_sav("/home/alvaro/Downloads/BD_EH2024/EH2024_Seguridad_Alimentaria.sav")
eh24v<-read_sav("/home/alvaro/Downloads/BD_EH2024/EH2024_Vivienda.sav")
save(eh24d,eh24e,eh24g,eh24p,eh24s,eh24v, file="_dataset/eh24.RData")
load("_dataset/eh24.RData")
#Población objetivo
bd<-eh24p %>% filter(s01a_03>=25)
#2. Establecer la relación interés
#Educación, área, sexo, edad, experiencia, horas trabajadas
bd<-bd %>% select(ylab, aestudio, area, s01a_02, s01a_03, tothrs,  totper, ynolab, cmasi, s01a_10, depto)
bd<-bd %>% na.omit()
bd<-bd %>% to_factor()
ggpairs(bd[,1:4]) #exploración visual
###################
summary(bd$ylab)
ggplot(bd,aes(ylab))+geom_histogram()
ggplot(bd,aes(ylab))+geom_boxplot()
ggplot(bd,aes(log(ylab)))+geom_histogram()
ggplot(bd,aes(log(ylab)))+geom_boxplot()
# X
ggplot(bd,aes(aestudio))+geom_bar()
ggplot(bd,aes(s01a_03))+geom_bar()
ggplot(bd,aes(area))+geom_bar()
ggplot(bd,aes(to_factor(s01a_02)))+geom_bar()
#3. Definir el modelo de interés
#modelo
m1<-lm(ylab~ aestudio,data = bd)
summary(m1)
plot(m1)

m2<-lm(log(ylab)~aestudio,data = bd)
summary(m2)
plot(m2)

m3<-lm(log(ylab)~ factor(aestudio),data = bd)
summary(m3)
plot(m3)

m4<-lm(log(ylab)~ .
       ,data = bd)
summary(m4)
plot(m4)
### Paso 4: Optimizar el modelo

#- Tratamiento sobre variables de control
#  + Transformaciones
#  + Definir como factor
#  + Polinomios
#  + Interacciones
#- Tratamiento de datos atípicos
#  + Bonferroni. H0: observación $i$ es 
#- Stepwise: Regresión paso a paso (step)
#- Backward: Regresión hacia atras
library(car)
library(mixlm)
residualPlots(m4)#HO: NO se requiere X^2
outlierTest(m4)
aux<-outlierTest(m4)
aux<-names(aux$rstudent)
bd1<-bd %>% slice(- as.numeric(aux))
bd1<-bd1 %>% mutate(aestudio=as.factor(aestudio))
influenceIndexPlot(m4, vars="Cook")
bd1$s01a_03<-as.numeric(bd1$s01a_03)
m5<-lm(log(ylab)~., data = bd1)
m6<-step(m5)
summary(m6)
m7<-backward(m5)
##a variables dummy 
library(fastDummies)
bd2<-dummy_cols(bd1, select_columns = c("aestudio"), remove_first_dummy = T, remove_selected_columns=T)
md1<-lm(log(ylab)~., data = bd2)
md2<-step(md1)
md3<-backward(md1, alpha = 0.05)
summary(md2)
summary(md3)
### Paso 5: Validar el modelo
#- Residuos
#  + Normalidad
#  + Varianza constante
#- Colinealidad
#  + VIF
#  + Inclusión de interacciones y polinomios

#normalidad
ee<-residuals(m5)
hist(ee)
#library(normtest)
library(nortest)# H0: Normalidad
ad.test(ee)
lillie.test(ee)

plot(density(scale(ee)))
curve(dnorm,add=T,col="red")
#Colinealidad
##Variance Inflation Factors
vif(m5)
sqrt(vif(m5)[,1])>2
# Verificar si la varianza es constante (homocedástico) o no (heterocedástico)
library(lmtest)
bptest(m5) # H0:  Homocedasticidad

#6. Analizar/Predecir a partir del modelo
ypred<-exp(predict(m5))
bd1$ylab[1]
ypred[1]
library(editData)
#base de datos nuevo
bdp<-bd1 %>% slice(1:2) %>% select(-ylab)
bdp<-editData(bdp)
exp(predict(m5,bdp))
