rm(list=ls())
library(tidymodels)
library(C50)
library(rules)      # Para C5.0
library(ranger)     # Para Random Forest
library(rpart.plot) # Para visualizar CART
library(vip)        # Importancia de variables
################################
#1. Recolección de datos: Identificación de variables estructurales X y la variable objetivo Y.
data("mlc_churn")
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
# Preprocesamiento
churn_recipe <- recipe(y ~ ., data = train_data) %>% 
  step_zv(all_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.9)
#CART
cart_spec <- decision_tree(cost_complexity = 0.01, tree_depth = 5) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

cart_wfl <- workflow() %>% add_recipe(churn_recipe) %>% add_model(cart_spec)
cart_fit <- cart_wfl %>% fit(data = train_data)

cart_fit %>% extract_fit_engine() %>% 
  rpart.plot(
    type = 5
  )

#C50
c50_spec <- boost_tree(trees = 10) %>% # trees > 1 activa el Boosting
  set_engine("C5.0") %>% 
  set_mode("classification")

c50_wfl <- workflow() %>% add_recipe(churn_recipe) %>% add_model(c50_spec)
c50_fit <- c50_wfl %>% fit(data = train_data)

#Random forest
rf_spec <- rand_forest(trees = 500, mtry = 4) %>% 
  set_engine("ranger", importance = "impurity") %>% # Importancia basada en Gini
  set_mode("classification")

rf_wfl <- workflow() %>% add_recipe(churn_recipe) %>% add_model(rf_spec)
rf_fit <- rf_wfl %>% fit(data = train_data)

################################
# 4. Evaluación del desempeño: Uso de métricas para verificar generalización [3, 15, 16].
# Predicciones sobre el conjunto de prueba 
# Función para recolectar resultados de prueba
eval_model <- function(model_fit, name) {
  test_data %>%
    dplyr::select(y) %>%
    bind_cols(predict(model_fit, test_data, type = "prob")) %>%
    bind_cols(predict(model_fit, test_data, type = "class")) %>% 
    mutate(model = name)
}
#Resultado conjunto
comp_results <- bind_rows(
  eval_model(cart_fit, "CART"),
  eval_model(c50_fit, "C5.0"),
  eval_model(rf_fit, "RF")
)

# Gráfico de Curvas ROC comparativas
comp_results %>%
  group_by(model) %>%
  roc_curve(truth = y, .pred_yes) %>%
  autoplot() +
  labs(title = "Duelo de Árboles: CART vs C5.0 vs Random Forest")

comp_results %>%
  group_by(model) %>%
  roc_auc(truth = y, .pred_yes)

# A. Matriz de Confusión 
comp_results %>% 
  filter(model=="CART") %>%
  conf_mat(truth = y, estimate = .pred_class) 

comp_results %>% 
  filter(model=="C5.0") %>%
  conf_mat(truth = y, estimate = .pred_class) 

comp_results %>% 
  filter(model=="RF") %>%
  conf_mat(truth = y, estimate = .pred_class) 

# B. Métricas de Calidad: Exactitud, Sensibilidad, Especificidad, F, Kappa precisión, recall 
eval_metrics <- metric_set(accuracy, sens, spec, f_meas, kap, precision, recall)
comp_results %>% 
  filter(model=="CART") %>%
  eval_metrics(truth = y, estimate = .pred_class)

comp_results %>% 
  filter(model=="C5.0") %>%
  eval_metrics(truth = y, estimate = .pred_class)

comp_results %>% 
  filter(model=="RF") %>%
  eval_metrics(truth = y, estimate = .pred_class)

####################################
#5. Mejora del modelo: Ajuste de parámetros o uso de métodos de ensamble para maximizar la precisión.
# Generar datos de umbrales
threshold_data <- comp_results %>% 
  filter(model=="RF") %>% 
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
best_threshold

### Opciones gráficas

cart_fit %>% extract_fit_engine()

# Extraer el motor del modelo C5.0
c50_engine <- c50_fit %>% extract_fit_engine()

# Ver las reglas en formato texto
summary(c50_engine)
