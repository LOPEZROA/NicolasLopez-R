install.packages(c("tidyverse", "nycflights13", "maps", "viridis"))

library(tidyverse)
library(nycflights13)
library(maps)    
library(viridis)  

df_viz <- flights %>%
  left_join(airlines, by = "carrier") %>%
  filter(!is.na(dep_time), !is.na(arr_time)) 


theme_set(theme_minimal(base_size = 12))


# =============================================================================
# 1. DISTRIBUCIONES (Histograma y Densidad)
# =============================================================================

# Gráfico A: Histograma con Curva de Densidad (Retrasos de Salida)
# Nota: Filtramos outliers extremos (>120 min) para ver mejor el grueso de datos
p1 <- df_viz %>%
  filter(dep_delay > -20, dep_delay < 120) %>%
  ggplot(aes(x = dep_delay)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "#69b3a2", color = "white", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  labs(
    title = "1. Distribución de Retrasos de Salida",
    subtitle = "Histograma + Densidad (Filtro: -20 a 120 min)",
    x = "Minutos de Retraso",
    y = "Densidad"
  )
print(p1)

# =============================================================================
# 2. COMPARACIÓN DE CATEGORÍAS (Boxplot y Violin Plot)
# =============================================================================

# Gráfico B: Boxplot Ordenado (Mejor para detectar medianas y outliers)
top_airlines <- df_viz %>% count(name, sort=TRUE) %>% head(10) %>% pull(name)

p2 <- df_viz %>%
  filter(name %in% top_airlines) %>%
  filter(arr_delay > -30, arr_delay < 120) %>% # Zoom para ver las cajas
  ggplot(aes(x = reorder(name, arr_delay, FUN = median), y = arr_delay, fill = name)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() + # Volteamos para leer mejor los nombres
  scale_fill_viridis(discrete = TRUE, option = "D", guide = "none") +
  labs(
    title = "2. Boxplot: Retrasos de Llegada por Aerolínea",
    subtitle = "Ordenado por mediana de retraso (Top 10 Aerolíneas)",
    x = "",
    y = "Minutos de Retraso"
  )
print(p2)

# =============================================================================
# 3. RELACIÓN ENTRE VARIABLES NUMÉRICAS (Scatter Plot)
# =============================================================================

# Gráfico C: Scatter Plot con Regresión Lineal
p3 <- df_viz %>%
  slice_sample(n = 5000) %>% # Muestreo aleatorio para no saturar el gráfico
  ggplot(aes(x = distance, y = air_time)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Línea de tendencia
  labs(
    title = "3. Correlación: Distancia vs Tiempo de Vuelo",
    subtitle = "Muestra de 5,000 vuelos",
    x = "Distancia (millas)",
    y = "Tiempo en aire (minutos)"
  )
print(p3)

# =============================================================================
# 4. SERIES DE TIEMPO (Line Chart)
# =============================================================================

# Gráfico D: Tendencia Diaria de Vuelos
p4 <- df_viz %>%
  group_by(date = as.Date(time_hour)) %>%
  summarise(total_vuelos = n()) %>%
  ggplot(aes(x = date, y = total_vuelos)) +
  geom_line(color = "#404080", size = 0.8) +
  geom_smooth(method = "loess", span = 0.2, color = "orange", fill = "gray90") + # Tendencia suavizada
  labs(
    title = "4. Serie de Tiempo: Cantidad de Vuelos Diarios (2013)",
    subtitle = "Línea naranja indica la tendencia suavizada (LOESS)",
    x = "Fecha",
    y = "Cantidad de Vuelos"
  )
print(p4)

# =============================================================================
# 5. ANÁLISIS MULTIVARIADO (Heatmap)
# Objetivo: Detectar patrones en dos dimensiones categóricas/temporales
# =============================================================================

# Gráfico E: Mapa de Calor (Día de la semana vs Hora del día)
p5 <- df_viz %>%
  mutate(
    dia_semana = wday(time_hour, label = TRUE, abbr = TRUE),
    hora_dia = hour
  ) %>%
  group_by(dia_semana, hora_dia) %>%
  summarise(promedio_retraso = mean(dep_delay, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = hora_dia, y = dia_semana, fill = promedio_retraso)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "magma", name = "Retraso (min)") +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "5. Heatmap: ¿Cuándo es peor volar?",
    subtitle = "Promedio de retraso por Hora y Día de la Semana",
    x = "Hora del Día",
    y = "Día de la Semana"
  )
print(p5)

# =============================================================================
# 6. COMPOSICIÓN (Bar Chart Apilado)
# =============================================================================

# Gráfico F: Barras apiladas al 100%
p6 <- df_viz %>%
  filter(name %in% top_airlines) %>%
  count(origin, name) %>%
  ggplot(aes(x = origin, y = n, fill = name)) +
  geom_bar(stat = "identity", position = "fill") + # Position fill para ver %
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Paired", name = "Aerolínea") +
  labs(
    title = "6. Composición: Market Share por Aeropuerto de Origen",
    subtitle = "Proporción de vuelos de las Top 10 aerolíneas en JFK, EWR, LGA",
    x = "Aeropuerto de Origen",
    y = "Proporción"
  )
print(p6)

# =============================================================================
# 7. FACETADO (Small Multiples)
# =============================================================================

# Gráfico G: Histograma facetado por origen
p7 <- df_viz %>%
  filter(dep_delay > -10, dep_delay < 60) %>%
  ggplot(aes(x = dep_delay, fill = origin)) +
  geom_histogram(binwidth = 5, color = "white", show.legend = FALSE) +
  facet_wrap(~origin) + # <--- Esto divide el gráfico en 3 paneles
  labs(
    title = "7. Facetado: Distribución de Retrasos por Aeropuerto",
    x = "Minutos de Retraso",
    y = "Frecuencia"
  )
print(p7)

# =============================================================================
# 8. GEOESPACIAL (Mapa de Rutas)
# =============================================================================

# Preparación de datos geográficos
usa_map <- map_data("usa")
aeropuertos <- nycflights13::airports

# Unimos vuelos con coordenadas de destino
vuelos_geo <- df_viz %>%
  group_by(dest) %>%
  summarise(total = n()) %>%
  inner_join(aeropuertos, by = c("dest" = "faa")) %>%
  filter(lon > -130, lat > 20) # Filtramos para quedarnos solo en USA continental

# Gráfico H: Mapa de conexiones desde NYC
p8 <- ggplot() +
  # Capa 1: El mapa base de USA
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), 
               fill = "gray90", color = "white") +
  # Capa 2: Puntos de los aeropuertos destino (Tamaño según tráfico)
  geom_point(data = vuelos_geo, aes(x = lon, y = lat, size = total), 
             color = "purple", alpha = 0.6) +
  # Capa 3: Etiqueta de NYC (Origen)
  annotate("text", x = -74, y = 40.7, label = "NYC", color = "red", fontface = "bold") +
  theme_void() + # Quitamos ejes y fondo
  labs(
    title = "8. Geoespacial: Destinos desde NYC",
    subtitle = "El tamaño del punto indica el volumen de vuelos",
    size = "Nro. Vuelos"
  )
print(p8)
