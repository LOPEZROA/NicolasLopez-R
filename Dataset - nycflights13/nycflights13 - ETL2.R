install.packages(c("tidyverse", "nycflights13"))

library(tidyverse)
library(nycflights13)

# --- 1. EXTRACT (Actualizado) ---
raw_flights <- nycflights13::flights
raw_weather <- nycflights13::weather  # <--- Nueva fuente de datos

# --- 2. TRANSFORM (Con Nuevas Métricas) ---
df_enriched <- raw_flights %>%
  filter(!is.na(dep_time)) %>%
  
  # A. INTEGRACIÓN DE DATOS (JOIN)
  # Cruzamos con datos del clima para saber qué pasaba en el origen a esa hora
  left_join(raw_weather, by = c("origin", "time_hour")) %>%
  
  mutate(
    # --- METRICA 1: Recuperación en Vuelo (Eficiencia del Piloto) ---
    # Si salió 30 min tarde pero llegó solo 10 min tarde, el piloto "recuperó" 20 min.
    # Fórmula: Retraso Salida - Retraso Llegada
    minutos_recuperados = dep_delay - arr_delay,
    
    # Flag para saber si hubo "esfuerzo de recuperación" (recuperó más de 5 min)
    piloto_acelero = if_else(minutos_recuperados > 5, TRUE, FALSE),
    
    # --- METRICA 2: Costo Operativo en Pista (Taxi Total) ---
    # Tiempo rodando en pista antes de despegar (taxi_out) y al aterrizar (taxi_in).
    # Esto es crítico porque el avión gasta combustible sin avanzar al destino.
    tiempo_total_pista = taxi_out + taxi_in,
    
    # Categoría de congestión en pista
    congestion_pista = case_when(
      tiempo_total_pista < 15 ~ "Fluido",
      tiempo_total_pista >= 15 & tiempo_total_pista < 30 ~ "Normal",
      tiempo_total_pista >= 30 ~ "Congestión Alta"
    ),
    
    # --- METRICA 3: Condiciones Adversas (Contexto) ---
    # Usamos las columnas de clima (visib = visibilidad en millas, precip = precipitación)
    condicion_despegue = case_when(
      visib < 5 ~ "Baja Visibilidad",
      precip > 0 ~ "Lluvia/Nieve",
      wind_gust > 25 ~ "Vientos Fuertes",
      TRUE ~ "Normal"
    )
  ) %>%
  
  # Selección final de columnas para el Data Warehouse
  select(
    fecha_hora = time_hour,
    nro_vuelo = flight,
    origen = origin,
    retraso_salida = dep_delay,
    retraso_llegada = arr_delay,
    # Nuevas columnas
    minutos_recuperados,
    piloto_acelero,
    tiempo_total_pista,
    congestion_pista,
    condicion_despegue,
    visibilidad_millas = visib
  )

# --- VER RESULTADOS ---
# Miremos los primeros registros de las nuevas métricas
head(df_enriched)