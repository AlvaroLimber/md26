rm(list=ls())
library(tidyverse)
library(tidymodels)
library(modeldata)
library(ExPanDaR)
################################
#1. Recolección de datos: Identificación de variables estructurales X y la variable objetivo Y.
#2. Exploración y Preparación (IDA/EDA): Limpieza, manejo de valores perdidos y normalización
#3. Entrenamiento del modelo: Selección del algoritmo adecuado basado en la naturaleza del problema.
#4. Evaluación del desempeño: Uso de métricas para verificar si el algoritmo aprendió patrones o solo memorizó los datos.
#5. Mejora del modelo: Ajuste de parámetros o uso de métodos de ensamble para maximizar la precisión.|