#Ejemplos de regresión según los pasos
#0. Pregunta de investigación, revisión de literatura 
# ¿Qué determina la pobreza en personas de 25 años o más?
#1. Base datos lista para el modelo (Unidad de investigación)
#2. Establecer la relación interés
#3. Definir el modelo de interés
#4. Optimizar el modelo
#5. Validar el modelo
#6. Analizar/Predecir a partir del modelo
rm(list=ls())
library(haven)
library(tidyverse)
library(margins)
library(labelled)
library(pscl)
library(car)
library(mixlm)
#data
load("_dataset/eh24.RData")
eh24p$p0 #Moderada (alimentaria + no alimentaria)
eh24p$pext0 #extrema (alimentaria)
unique(eh24p$zext)
unique(eh24p$z)
#Población objetivo: Personas de 25 años o más
bd<-eh24p %>% 
  mutate(pobreza=(p0==1)) %>% 
  filter(s01a_03>=25)
# X? Rev. literatura, Inferencia variacional
bd<-bd %>% mutate(aestudio=to_factor(aestudio),
                  rural=(area==2),
                  depto=to_factor(depto),
                  mujer=(s01a_02==2),
                  edad=as.numeric(s01a_03),
                  ecivil=to_factor(s01a_10)
                  ) %>% select(pobreza, aestudio, rural, depto, mujer, edad, ecivil)
bd<-bd %>% na.omit()
#NOTA: Se debe realizar un proceso de exploración previa a la modelación
prop.table(table(bd$pobreza))*100
bd<-bd %>% slice(-3458)
m1<-glm(pobreza ~ ., 
        data = bd, 
        family=binomial(link="logit"))
m2<-glm(pobreza ~ ., 
        data=bd, 
        family=binomial(link="probit"))
#step(m1)
m1
m2
mm1<-margins(m1)
smm1<-summary(mm1)
plot(mm1)

mm2<-margins(m2)
smm2<-summary(mm2)
plot(mm2)

round(pR2(m1), 3)
round(pR2(m2), 3)

outlierTest(m1)
influenceIndexPlot(m1, vars="Cook")
vif(m1)
