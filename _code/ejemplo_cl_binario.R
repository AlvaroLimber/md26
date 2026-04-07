rm(list=ls())
library(tidyverse)
library(tidymodels)# https://rstudio.github.io/cheatsheets/ml-preprocessing-data.pdf
library(modeldata)
library(ExPanDaR)
library(probably)
################################
#1. Recolección de datos: Identificación de variables estructurales X y la variable objetivo Y.
data("mlc_churn") # ?modeldata::mlc_churn 
bd<-mlc_churn
################################
#2. Exploración y Preparación (IDA/EDA): Limpieza, manejo de valores perdidos y normalización
glimpse(bd)#resumen rápido
#ExPanD(bd)
bd<-bd %>% rename(y=churn)
#partición
set.seed(1122)
data_split <- initial_split(bd, prop = 0.80, strata = y)
train_data <- training(data_split)
test_data  <- testing(data_split)
################################
# 3. Entrenamiento del modelo: Selección del algoritmo adecuado basado en la naturaleza del problema.
logit_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

probit_spec <- logistic_reg() %>% 
  set_engine("glm", family = binomial(link = "probit")) %>% 
  set_mode("classification")
# Definición de la receta (Preprocesamiento)
churn_recipe <- recipe(y ~ ., data = train_data) %>% 
  step_rm() %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# Creación del Workflow (Flujo de trabajo)
churn_wfl <- workflow() %>% 
  add_model(logit_spec) %>% 
  add_recipe(churn_recipe)

churn_wfp <- workflow() %>% 
  add_model(logit_spec) %>% 
  add_recipe(churn_recipe)

# Ajuste del modelo sobre los datos de entrenamiento
logit_fit <- churn_wfl %>% 
  fit(data = train_data)

probit_fit <- churn_wfp %>% 
  fit(data = train_data)

################################
# 4. Evaluación del desempeño: Uso de métricas para verificar generalización [3, 15, 16].
# Predicciones sobre el conjunto de prueba 
results <- test_data %>%
  select(y) %>%
  bind_cols(predict(logit_fit, test_data, type = "class")) %>%
  bind_cols(predict(logit_fit, test_data, type = "prob"))

# A. Matriz de Confusión 
results %>% 
  conf_mat(truth = y, estimate = .pred_class) 

# B. Métricas de Calidad: Exactitud, Sensibilidad, Especificidad y Kappa 
eval_metrics <- metric_set(accuracy, sens, spec, kap, precision, recall)
results %>% 
  eval_metrics(truth = y, estimate = .pred_class)

# C. Curva ROC y AUC
# El AUC nos da la probabilidad de que el modelo discrimine correctamente entre clases .
results %>% 
  roc_curve(truth = y, .pred_yes) %>% 
  autoplot()

results %>% 
  roc_auc(truth = y, .pred_yes)

results %>%
  gain_curve(truth = y, .pred_yes) %>%
  autoplot() +
  labs(title = "Curva de Ganancia: ¿Qué tan rápido encontramos a los desertores?")

# Gráfico de Elevación (Lift)
results %>%
  lift_curve(truth = y, .pred_yes) %>%
  autoplot() +
  labs(title = "Curva de Lift: Efectividad del Modelo vs. Azar")

results %>%
  ggplot(aes(x = .pred_yes, fill = y)) +
  geom_density(alpha = 0.5) +
  labs(title = "Separación de Clases", x = "Probabilidad Predicha (Yes)", y = "Densidad") +
  theme_minimal()

# Generar datos de umbrales
threshold_data <- results %>%
  threshold_perf(truth = y, estimate = .pred_yes, thresholds = seq(0, 1, by = 0.01))

# Graficar Sensibilidad, Especificidad y J-Index
threshold_data %>%
  filter(.metric %in% c("sens", "spec", "j_index")) %>%
  ggplot(aes(x = .threshold, y = .estimate, color = .metric)) +
  geom_line() +
  geom_vline(xintercept = 0.5, lty = 2, color = "gray50") +
  labs(title = "Evaluación de Umbrales de Probabilidad",
       x = "Umbral de Corte", y = "Valor de la Métrica") +
  theme_minimal()

# Encontrar el umbral óptimo matemáticamente
best_threshold <- threshold_data %>%
  filter(.metric == "j_index") %>%
  slice_max(.estimate) %>%
  pull(.threshold)

cat("El umbral óptimo basado en J-Index es:", best_threshold)

# Ejemplo: Aplicar el nuevo umbral a tus resultados
results_final <- results %>%
  mutate(.pred_class_custom = make_two_class_pred(.pred_yes, levels(y), threshold = best_threshold))

# Nueva Matriz de Confusión con el corte optimizado
results_final %>%
  conf_mat(truth = y, estimate = .pred_class_custom)
