rm(list=ls())
library(tidyverse)
library(haven)#importar
library(readxl)#EXCEL
#######################
tics<-read_excel("C:/Users/Clases/Downloads/3020601.xlsx",
                 range = "B5:CN1407")
class(tics)
View(tics)
data("USArrests")
View(USArrests)
names(USArrests)
names(tics)
#renombrado
tics <- tics %>% rename( depto = DEPARTAMENTO,
                 nro = 1 )
names(tics) <- tolower(names(tics))

names(tics)[7:13]
raiz<-c("total","radio","noradio","tv","notv","telf","notelf")
paste0(raiz, "_01")

names(tics)[7:13]<-paste0(raiz, "_01")
aux<-sort(rep(c("_01", "_12", "_24"),7))
paste0(raiz, aux)

#filtrado
tics<-tics %>% filter(nivel=="Municipio/TIOC")

#Tarea: 
  - Renombras de forma completa la tabla
  - Filtrar solo información del municipio (no urbano/rural)
  - Matriz de correlación para un año en específico (totales/%)
  



