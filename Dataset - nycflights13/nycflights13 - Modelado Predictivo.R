install.packages(c("tidyverse", "nycflights13", "caret", "car", "ranger", "lmtest"))


library(tidyverse)
library(nycflights13)
library(caret)      
library(car)        
library(lmtest)     
library(ranger)     

set.seed(123) 
View(flights$month)

# Unimos vuelos con clima y aerolíneas
data_model <- flights %>%
  inner_join(weather, by = c("origin", "time_hour")) %>%
  inner_join(airlines, by = "carrier") %>%
  
  select(
    arr_delay,          
    dep_delay,          
    month= month.x,              
    distance,           
    carrier = name,     
    origin,             
    visib,              
    wind_speed,         
    precip              
  ) %>%
  
 
  filter(!is.na(arr_delay)) %>%   
  na.omit() %>%                   
  
 
  mutate(
    carrier = as.factor(carrier),
    origin = as.factor(origin),
    month = as.factor(month)      
  )



# =============================================================================
# 2. SPLITTING (Entrenamiento vs Prueba)
# =============================================================================
# Usamos 80% para entrenar, 20% para validar
index <- createDataPartition(data_model$arr_delay, p = 0.8, list = FALSE)
train_set <- data_model[index, ]
test_set  <- data_model[-index, ]

# =============================================================================
# 3. MODELO 1: REGRESIÓN LINEAL MÚLTIPLE (Enfoque Estadístico)
# =============================================================================
model_lm <- lm(arr_delay ~ ., data = train_set)

# Resumen rápido
summary_lm <- summary(model_lm)
cat(paste("R-Squared Ajustado:", round(summary_lm$adj.r.squared, 4), "\n"))

# =============================================================================
# 4. DIAGNÓSTICO DE SUPUESTOS (CRÍTICO PARA INGENIERÍA ESTADÍSTICA)
# =============================================================================
# A. MULTICOLINEALIDAD (VIF)
# Si VIF > 5 o 10, hay problemas graves de correlación entre predictores.
vif_values <- car::vif(model_lm)
print(vif_values[,1]) 

if(any(vif_values[,1] > 5)) {
  cat("[ALERTA] Algunas variables tienen VIF alto. Considera removerlas.\n")
} else {
  cat("[OK] No se detecta multicolinealidad severa.\n")
}

# B. NORMALIDAD DE LOS RESIDUOS

# Usamos inspección visual (Q-Q Plot)
par(mfrow = c(2, 2)) 
plot(model_lm)       
par(mfrow = c(1, 1)) 

# C. HOMOCEDASTICIDAD (Test de Breusch-Pagan)
# H0: Varianza constante (Homocedasticidad) vs H1: Heterocedasticidad
bp_test <- bptest(model_lm)
print(bp_test)

# =============================================================================
# 5. MODELO 2: RANDOM FOREST (Enfoque Machine Learning)
# =============================================================================
model_rf <- ranger(
  formula = arr_delay ~ ., 
  data = train_set, 
  num.trees = 100,       # 100 árboles para prueba rápida
  importance = 'impurity',
  verbose = TRUE
)

print(model_rf)

# Importancia de Variables
print(sort(importance(model_rf), decreasing = TRUE)[1:5])

# =============================================================================
# 6. EVALUACIÓN Y COMPARACIÓN DE RENDIMIENTO
# =============================================================================


# Predicciones
pred_lm <- predict(model_lm, newdata = test_set)
pred_rf <- predict(model_rf, data = test_set)$predictions

# Función auxiliar para métricas
calc_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- cor(actual, predicted)^2
  return(c(RMSE = rmse, MAE = mae, R2 = r2))
}

metrics_lm <- calc_metrics(test_set$arr_delay, pred_lm)
metrics_rf <- calc_metrics(test_set$arr_delay, pred_rf)

# Tabla Comparativa
comparison <- rbind(Regresion_Lineal = metrics_lm, Random_Forest = metrics_rf)
print(round(comparison, 3))

