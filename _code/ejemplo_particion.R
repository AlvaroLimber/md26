rm(list=ls())
library(tidyverse)
library(tidymodels)
library(modeldata)
library(ExPanDaR)
library(themis)
#######################
data("mlc_churn") # ?modeldata::mlc_churn 
data("cat_adoption") # ?modeldata::cat_adoption
#######################
#Churn
bd<-mlc_churn
glimpse(bd)#resumen rápido
#ExPanD(bd)
#partición
set.seed(1122)
data_split <- initial_split(bd, prop = 0.80, strata = churn)
train_data <- training(data_split)
test_data  <- testing(data_split)
#verificación
prop.table(table(bd$churn))
prop.table(table(train_data$churn))
prop.table(table(test_data$churn))
#######################
#Desbalanceo
#######################
# Opción A: Estrategia SMOTE (Sobremuestreo Sintético) - Recomendada
receta_smote <- recipe(churn ~ ., data = train_data) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_smote(churn, over_ratio = 1)              

# Opción B: Estrategia Downsampling (Submuestreo) - Alternativa
receta_down <- recipe(churn ~ ., data = train_data) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_downsample(churn, under_ratio = 1)        

# Verificación del Balanceo
train_smote <- prep(receta_smote) %>% bake(new_data = NULL)
train_down  <- prep(receta_down) %>% bake(new_data = NULL)

prop.table(table(train_smote$churn))
table(train_smote$churn)

prop.table(table(train_down$churn))
table(train_down$churn)
#######################
#Adopción
bd<-cat_adoption
glimpse(bd)#resumen rápido
#partición
set.seed(1122)
data_split <- initial_split(bd, prop = 0.80, strata = event)
train_data <- training(data_split)
test_data  <- testing(data_split)
#verificación
prop.table(table(bd$event))
prop.table(table(train_data$event))
prop.table(table(test_data$event))
##############################################################
