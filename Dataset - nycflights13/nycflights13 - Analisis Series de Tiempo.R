install.packages(c("tidyverse", "nycflights13", "forecast", "tseries", "lubridate"))

# =============================================================================
# ANÁLISIS DE SERIES DE TIEMPO: DEMANDA DIARIA DE VUELOS (NYC 2013)
# =============================================================================

library(tidyverse)
library(nycflights13)
library(lubridate)
library(forecast)  
library(tseries)  


# A. Agregación Diaria
# Convertimos datos transaccionales en una serie de tiempo de "Conteo Diario"
daily_flights <- flights %>%
  mutate(date = as.Date(time_hour)) %>%
  group_by(date) %>%
  summarise(total_vuelos = n()) %>%
  ungroup()

# B. Manejo de Fechas Faltantes (Gap Filling)
# Es vital asegurar que no falten días en la secuencia temporal
rango_completo <- seq(min(daily_flights$date), max(daily_flights$date), by = "day")
daily_flights <- daily_flights %>%
  complete(date = rango_completo, fill = list(total_vuelos = 0))

# C. Creación del Objeto TS
ts_vuelos <- ts(daily_flights$total_vuelos, frequency = 7)

print(head(daily_flights))

# =============================================================================
# 2. DESCOMPOSICIÓN DE LA SERIE (STL)
# =============================================================================

# Usamos s.window = "periodic" para forzar una estacionalidad constante
fit_stl <- stl(ts_vuelos, s.window = "periodic")

# Visualización de la descomposición
plot(fit_stl, main = "Descomposición STL: Demanda Diaria de Vuelos")

# =============================================================================
# 3. ANÁLISIS DE ESTACIONARIEDAD
# =============================================================================

# A. Visualización de Autocorrelación (ACF) y Parcial (PACF)
par(mfrow = c(1, 2))
acf(ts_vuelos, main = "ACF (Autocorrelación)", lag.max = 30)
pacf(ts_vuelos, main = "PACF (Autocorrelación Parcial)", lag.max = 30)
par(mfrow = c(1, 1))

# B. Test Aumentado de Dickey-Fuller (ADF)
# H0: La serie tiene raíz unitaria (No es estacionaria)
# H1: La serie es estacionaria
adf_res <- adf.test(ts_vuelos, alternative = "stationary")

print(adf_res)

if(adf_res$p.value < 0.05) {
  cat("[RESULTADO] p-value < 0.05: Rechazamos H0. La serie ES Estacionaria.\n")
} else {
  cat("[RESULTADO] p-value >= 0.05: No rechazamos H0. La serie NO es estacionaria (requiere diferenciación 'd').\n")
}

# =============================================================================
# 4. MODELADO (ARIMA)
# =============================================================================

# auto.arima busca el mejor modelo (p,d,q)(P,D,Q)[m] minimizando el AICc
# stepwise = FALSE y approximation = FALSE hacen una búsqueda más lenta pero exhaustiva
modelo_arima <- auto.arima(ts_vuelos, 
                           seasonal = TRUE, 
                           stepwise = FALSE, 
                           approximation = FALSE,
                           trace = TRUE) 

summary(modelo_arima)

# =============================================================================
# 5. DIAGNÓSTICO DE RESIDUOS
# =============================================================================
# Gráficos de diagnóstico automático de 'forecast'
checkresiduals(modelo_arima)

# Test de Ljung-Box formal
# H0: Los residuos se distribuyen aleatoriamente (Ruido Blanco)
# H1: Los residuos tienen autocorrelación (El modelo no capturó toda la señal)
residuos_test <- checkresiduals(modelo_arima, plot = FALSE)
print(residuos_test)

if(residuos_test$p.value > 0.05) {
  cat("[VALIDACIÓN] p-value > 0.05: Residuos son Ruido Blanco. Modelo Válido.\n")
} else {
  cat("[ALERTA] p-value < 0.05: Aún queda estructura en los residuos. Considerar ajustar parámetros manualmente.\n")
}

# =============================================================================
# 6. FORECASTING (PROYECCIÓN)
# =============================================================================
# Proyectamos los siguientes 30 días (Enero 2014)
proyeccion <- forecast(modelo_arima, h = 30)

# Graficamos la serie original + la proyección
autoplot(proyeccion) +
  labs(title = "Pronóstico de Vuelos Diarios (Próximos 30 días)",
       x = "Tiempo (Semanas)",
       y = "Cantidad de Vuelos") +
  theme_minimal() +
  theme(legend.position = "bottom")
