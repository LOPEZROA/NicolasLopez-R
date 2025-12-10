install.packages(c("tidyverse", "nycflights13", "prophet", "lubridate"))

# =============================================================================
# FORECASTING AVANZADO DE VUELOS CON FACEBOOK PROPHET
# =============================================================================

library(tidyverse)
library(nycflights13)
library(lubridate)
library(prophet)    

# Prophet requiere estrictamente dos columnas:
# 'ds': La fecha (datestamp)
# 'y':  La variable métrica a predecir

df_prophet <- flights %>%
  mutate(date = as.Date(time_hour)) %>%
  group_by(date) %>%
  summarise(y = n()) %>%  
  rename(ds = date) %>%
  ungroup()

# Eliminamos posibles días vacíos si existen 
cat(paste("Rango de fechas:", min(df_prophet$ds), "a", max(df_prophet$ds), "\n"))
cat(paste("Total observaciones:", nrow(df_prophet), "\n"))

# =============================================================================
# 2. CONFIGURACIÓN DEL MODELO# =============================================================================

# Configuramos Prophet habilitando estacionalidad diaria y feriados de USA.
# 'daily.seasonality': FALSE porque nuestros datos son diarios, no sub-diarios.
# 'weekly.seasonality': TRUE para capturar el patrón Lunes-Domingo.
# 'yearly.seasonality': TRUE para capturar verano/invierno.

m <- prophet(
  daily.seasonality = FALSE,
  weekly.seasonality = TRUE,
  yearly.seasonality = TRUE,
  seasonality.mode = 'multiplicative' # El tráfico suele variar porcentualmente, no aditivamente
)

# Agregamos los feriados de Estados Unidos (CRÍTICO para datos de vuelos)
m <- add_country_holidays(m, country_name = 'US')

# Ajustamos el modelo
m <- fit.prophet(m, df_prophet)

# Ver qué feriados detectó
print(head(m$train.holiday.names, 10))

# =============================================================================
# 3. GENERACIÓN DEL FUTURO Y PREDICCIÓN
# =============================================================================

# Creamos un dataframe "vacío" con las fechas futuras
future <- make_future_dataframe(m, periods = 90, freq = "day")

# Realizamos la predicción
# 'yhat': La predicción puntual
# 'yhat_lower' / 'yhat_upper': Intervalos de incertidumbre
forecast <- predict(m, future)

# Mostramos las últimas 5 predicciones
print(tail(select(forecast, ds, yhat, yhat_lower, yhat_upper)))

# =============================================================================
# 4. VISUALIZACIÓN DE COMPONENTES
# =============================================================================
# Gráfico principal: Datos reales (puntos negros) + Predicción (línea azul)
plot(m, forecast) +
  labs(title = "Pronóstico de Demanda de Vuelos (NYC)",
       y = "Cantidad de Vuelos Diarios", x = "Fecha") +
  theme_minimal()

# Gráfico de componentes: Tendencia, Semanal, Anual y Feriados
prophet_plot_components(m, forecast)

# =============================================================================
# 5. VALIDACIÓN CRUZADA (BACKTESTING)
# =============================================================================
# initial: Días de entrenamiento inicial
# period: Cada cuánto re-entrenamos el modelo (corte)
# horizon: A cuántos días predecimos en cada corte
df_cv <- cross_validation(m, initial = 180, period = 30, horizon = 30, units = 'days')

# Calculamos métricas de performance (RMSE, MAE, MAPE)
df_p <- performance_metrics(df_cv)

print(colMeans(select(df_p, rmse, mae, mape)))

# Visualizamos el error (MAPE) a medida que aumenta el horizonte de pronóstico
plot_cross_validation_metric(df_cv, metric = 'mape') +
  labs(title = "MAPE vs Horizonte de Pronóstico", 
       subtitle = "¿Qué tanto se degrada la predicción al alejarnos en el tiempo?")
