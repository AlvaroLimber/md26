#Datasets para asociaciones
rm(list=ls())
library(tidyverse)
library(arules)
library(haven)
library(labelled)
###############################
#Listas
lista_compras <- list(
  c("pan", "leche"),
  c("pan", "pañales", "cerveza", "huevos"),
  c("leche", "pañales", "cerveza", "cola"),
  c("pan", "leche", "pañales", "cerveza")
)
transacciones <- as(lista_compras, "transactions")
###############################
# Matriz lógica
matriz_logica <- matrix(c(
  TRUE,  TRUE,  FALSE,
  TRUE,  FALSE, TRUE,
  FALSE, TRUE,  TRUE
), nrow = 3, byrow = TRUE)

colnames(matriz_logica) <- c("Item1", "Item2", "Item3")
transacciones <- as(matriz_logica, "transactions")
##############################
# Split
datos_long <- data.frame(
  TID = c(1, 1, 2, 2, 2),
  Item = c("manzana", "pera", "manzana", "naranja", "leche")
)
transacciones <- as(split(datos_long$Item, datos_long$TID), "transactions")
##############################
# Encuesta a transacción
load("_dataset/eh24.RData")
bd<-eh24p %>% select(p0, niv_ed_g, area, depto, s01a_02) %>% 
  na.omit() %>% as_factor()
transacciones<-as(bd, "transactions")
##############################
#Explorar transacciones
data("Groceries")
bdt<-Groceries
summary(bdt)
inspect(bdt[1:2])
itemFrequency(bdt[,1:2])
itemFrequencyPlot(bdt, topN=20)
hist(size(bdt))
image(sample(bdt, 100))
ct <- crossTable(bdt, sort = TRUE)
ct[1:5, 1:5]

