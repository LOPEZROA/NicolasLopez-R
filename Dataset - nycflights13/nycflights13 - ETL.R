install.packages(c("tidyverse", "nycflights13"))


library(tidyverse)    
library(nycflights13) 

# Definimos rutas de salida 
ruta_csv_salida <- "reporte_vuelos_procesado.csv"
nombre_db_salida <- "data_warehouse_vuelos.sqlite"

# -----------------------------------------------------------------------------
# 1. EXTRACT (Extracción)
# -----------------------------------------------------------------------------

# Aquí extraemos los dataframes del paquete.
raw_flights  <- nycflights13::flights
raw_airlines <- nycflights13::airlines
raw_planes   <- nycflights13::planes

cat(paste("    - Registros de vuelos extraídos:", nrow(raw_flights), "\n"))
cat(paste("    - Aerolíneas extraídas:", nrow(raw_airlines), "\n"))

# -----------------------------------------------------------------------------
# 2. TRANSFORM (Transformación)
# -----------------------------------------------------------------------------
cat("\n[2] Transformando y limpiando datos...\n")

df_transformed <- raw_flights %>%
  # A. LIMPIEZA INICIAL
  # Eliminamos vuelos cancelados (aquellos que no tienen tiempo de salida)
  filter(!is.na(dep_time)) %>% 
  
  # B. ENRIQUECIMIENTO 
  # Agregamos el nombre completo de la aerolínea
  left_join(raw_airlines, by = "carrier") %>%
  # Agregamos detalles del avión (año de fabricación)
  left_join(select(raw_planes, tailnum, plane_year = year), by = "tailnum") %>%
  
  # C. SELECCIÓN Y RENOMBRADO
  select(
    fecha_vuelo = time_hour,
    aerolinea = name,
    nro_vuelo = flight,
    origen = origin,
    destino = dest,
    retraso_salida = dep_delay,
    retraso_llegada = arr_delay,
    tiempo_aire = air_time,
    distancia = distance,
    anio_avion = plane_year
  ) %>%
  
  # D. CREACIÓN DE NUEVAS MÉTRICAS (Feature Engineering)
  mutate(
    # 1. Calcular velocidad promedio (millas por hora)
    velocidad_mph = round(distancia / (tiempo_aire / 60), 2),
    
    # 2. Categorizar el retraso (KPI de negocio)
    estado_vuelo = case_when(
      retraso_llegada <= 0 ~ "A tiempo / Anticipado",
      retraso_llegada > 0 & retraso_llegada <= 15 ~ "Retraso Leve",
      retraso_llegada > 15 & retraso_llegada <= 60 ~ "Retraso Moderado",
      retraso_llegada > 60 ~ "Retraso Severo",
      TRUE ~ "Desconocido"
    ),
    
    # 3. Calcular antigüedad del avión al momento del vuelo (2013)
    antiguedad_avion = 2013 - anio_avion
  ) %>%
  
  # E. MANEJO FINAL DE NULOS
  # Rellenamos la antigüedad desconocida con el promedio (imputación simple) o 0
  replace_na(list(antiguedad_avion = 0, velocidad_mph = 0))

# Creamos una segunda tabla agregada (Resumen por Aerolínea)
df_resumen_aerolinea <- df_transformed %>%
  group_by(aerolinea) %>%
  summarise(
    total_vuelos = n(),
    promedio_retraso_llegada = round(mean(retraso_llegada, na.rm = TRUE), 1),
    porcentaje_retrasos_severos = round(sum(estado_vuelo == "Retraso Severo") / n() * 100, 2)
  ) %>%
  arrange(desc(total_vuelos))

cat(paste("    - Filas limpias resultantes:", nrow(df_transformed), "\n"))

# -----------------------------------------------------------------------------
# 3. LOAD (Carga)
# -----------------------------------------------------------------------------
# DESTINO A: Archivo Plano (CSV) para reporte
write_csv(df_transformed, ruta_csv_salida)

