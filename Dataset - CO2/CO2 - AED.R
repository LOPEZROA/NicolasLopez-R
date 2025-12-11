# =============================================================================
# AED
# =============================================================================
install.packages(c("tidyverse","GGally","gridExtra"))

library(tidyverse)
library(GGally) 
library(gridExtra)

data("CO2")

# 1. Resumen Tabular Robusto
# Calculamos Media, Desviación Estándar y Error Estándar por grupos
resumen_desc <- CO2 %>%
  group_by(Type, Treatment) %>%
  summarise(
    n = n(),
    uptake_mean = mean(uptake),
    uptake_sd = sd(uptake),
    uptake_se = sd(uptake) / sqrt(n()),
    .groups = "drop"
  )

print(resumen_desc)

# 2. Visualización de la Variable Respuesta
# Boxplot con puntos (Jitter) para ver distribución y outliers
p1 <- ggplot(CO2, aes(x = Type, y = uptake, fill = Treatment)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribución de Uptake por Tipo y Tratamiento",
       y = "CO2 Uptake rates")

# 3. Gráfico de Interacción (Critical para Diseño Factorial)
# Nos dice si el efecto del tratamiento depende del origen de la planta
p2 <- CO2 %>%
  group_by(Type, Treatment, conc) %>%
  summarise(mean_uptake = mean(uptake), .groups = 'drop') %>%
  ggplot(aes(x = conc, y = mean_uptake, color = Type, linetype = Treatment)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Gráfico de Interacción: Curvas de Respuesta",
       subtitle = "Evidencia de saturación (no linealidad)",
       x = "Concentración CO2", y = "Uptake Promedio")

# Mostrar gráficos
grid.arrange(p1, p2, ncol = 2)
