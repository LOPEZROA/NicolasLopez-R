# -----------------------------------------------------------------------------
# Dataset: nycflights13
# -----------------------------------------------------------------------------

library(tidyverse)
library(nycflights13)
library(corrplot) 

# Cargamos el dataset en memoria
df_vuelos <- nycflights13::flights


# -----------------------------------------------------------------------------
# 1. INSPECCIÓN ESTRUCTURAL Y DUPLICADOS
# -----------------------------------------------------------------------------
glimpse(df_vuelos)

# Verificación de duplicados exactos (Filas repetidas)
duplicados <- df_vuelos %>% 
  group_by(year, month, day, flight, carrier, time_hour) %>% 
  filter(n() > 1)

if(nrow(duplicados) == 0) {
  cat("\n[OK] No se encontraron registros duplicados (Primary Key candidata respetada).\n")
} else {
  cat(paste("\n[ALERTA] Se encontraron", nrow(duplicados), "registros duplicados.\n"))
}

# -----------------------------------------------------------------------------
# 2. ANÁLISIS DE VALORES PERDIDOS (MISSING VALUES)
# -----------------------------------------------------------------------------

# Tabla resumen de NAs por columna (solo aquellas que tienen NAs)
tabla_nas <- df_vuelos %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Nulos") %>%
  filter(Nulos > 0) %>%
  mutate(Porcentaje = round((Nulos / nrow(df_vuelos)) * 100, 2)) %>%
  arrange(desc(Nulos))

print(tabla_nas)

# -----------------------------------------------------------------------------
# 3. ESTADÍSTICA DESCRIPTIVA (VARIABLES NUMÉRICAS)
# -----------------------------------------------------------------------------
# Seleccionamos variables de interés para distribución (Retrasos y Tiempos)
stats_resumen <- df_vuelos %>%
  select(dep_delay, arr_delay, air_time, distance) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
  group_by(Variable) %>%
  summarise(
    Min = min(Valor, na.rm = TRUE),
    Q1  = quantile(Valor, 0.25, na.rm = TRUE),
    Media = round(mean(Valor, na.rm = TRUE), 2),
    Mediana = median(Valor, na.rm = TRUE),
    Q3  = quantile(Valor, 0.75, na.rm = TRUE),
    Max = max(Valor, na.rm = TRUE),
    Desv_Std = round(sd(Valor, na.rm = TRUE), 2),
    N_Registros = n()
  )

print(stats_resumen)

# -----------------------------------------------------------------------------
# 4. DETECCIÓN DE OUTLIERS (MÉTODO IQR)
# -----------------------------------------------------------------------------

# Filtramos solo los retrasos de llegada para analizar valores extremos
outliers_llegada <- df_vuelos %>%
  filter(!is.na(arr_delay)) %>%
  select(carrier, flight, arr_delay) %>%
  mutate(
    Q1 = quantile(arr_delay, 0.25),
    Q3 = quantile(arr_delay, 0.75),
    IQR = Q3 - Q1,
    Limite_Superior = Q3 + 1.5 * IQR,
    Es_Outlier = arr_delay > Limite_Superior
  ) %>%
  filter(Es_Outlier) %>%
  arrange(desc(arr_delay))

cat(paste("Cantidad de vuelos considerados outliers por retraso de llegada:", nrow(outliers_llegada), "\n"))
cat("Top 5 Retrasos más extremos (Minutos):\n")
print(head(select(outliers_llegada, carrier, flight, arr_delay), 5))

# -----------------------------------------------------------------------------
# 5. VERIFICACIÓN DE CONSISTENCIA LÓGICA (BUSINESS LOGIC)
# -----------------------------------------------------------------------------

# Check A: ¿Hay tiempos de vuelo negativos? (Imposible físicamente)
vuelos_negativos <- df_vuelos %>% filter(air_time < 0)
cat(paste("Check A - Vuelos con tiempo negativo:", nrow(vuelos_negativos), "\n"))

# Check B: Vuelos que "llegaron" antes de salir (sin contar cambio de zona horaria mal ajustado)
# Nota: Esto es complejo por zonas horarias, pero arr_time < dep_time en el mismo día es sospechoso si no es overnight.
# Verificamos simplemente si hay incoherencia masiva en datos limpios.
check_tiempos <- df_vuelos %>%
  filter(!is.na(arr_time) & !is.na(dep_time)) %>%
  mutate(error_logico = if_else(arr_time < dep_time & air_time < 600, TRUE, FALSE)) %>% # Heurística simple
  filter(error_logico == TRUE)

cat(paste("Check B - Posibles inconsistencias horarias (Overnight flags):", nrow(check_tiempos), "\n"))

# -----------------------------------------------------------------------------
# 6. TABLAS RESUMEN (PIVOT TABLES) PARA REPORTE
# -----------------------------------------------------------------------------

# Tabla 1: Performance por Aeropuerto de Origen
tabla_aeropuertos <- df_vuelos %>%
  group_by(origin) %>%
  summarise(
    Total_Vuelos = n(),
    Tasa_Cancelacion = round(sum(is.na(dep_time)) / n() * 100, 2),
    Promedio_Retraso_Salida = round(mean(dep_delay, na.rm = TRUE), 1)
  )
print(tabla_aeropuertos)

# Tabla 2: Matriz de Correlación (Variables Numéricas)
matriz_cor <- df_vuelos %>%
  select(dep_delay, arr_delay, air_time, distance) %>%
  na.omit() %>%
  cor()

print(round(matriz_cor, 2))
