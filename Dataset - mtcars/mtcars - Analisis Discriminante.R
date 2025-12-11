# =============================================================================
# ANÁLISIS DISCRIMINANTE (LDA y QDA)
# =============================================================================

# 1. LIBRERÍAS
install.packages("tidyverse","MASS","heplots","klaR","caret","gridExtra")
library(tidyverse)
library(MASS)       
library(heplots)    
library(klaR)       
library(caret)      

data("mtcars")

# 2. PREPARACIÓN DE DATOS
f_da <- mtcars %>%
  dplyr::select(cyl, mpg, disp) %>%
  mutate(cyl = factor(cyl)) # Importante: La clase debe ser Factor

# Visualizamos la separación natural de las clases
ggplot(df_da, aes(x = mpg, y = disp, color = cyl)) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", level = 0.95) + # Elipses de confianza (asumiendo normalidad)
  labs(title = "Espacio de Clasificación: Mpg vs Disp",
       subtitle = "Observa las elipses: ¿Tienen la misma forma/orientación? (Supuesto LDA)",
       color = "Cilindros") +
  theme_minimal()


# =============================================================================
# 3. VERIFICACIÓN DE SUPUESTOS (CRÍTICO PARA ELEGIR LDA vs QDA)
# =============================================================================

box_res <- boxM(df_da[, c("mpg", "disp")], df_da$cyl)
print(box_res)

# Interpretación automática para el script
if(box_res$p.value < 0.05) {
  cat("--> RESULTADO: Se rechaza H0. Las covarianzas son distintas.\n")
  cat("--> RECOMENDACIÓN TÉCNICA: QDA es superior teóricamente.\n")
} else {
  cat("--> RESULTADO: No se rechaza H0. Covarianzas homogéneas.\n")
  cat("--> RECOMENDACIÓN TÉCNICA: LDA es suficiente (Principio de Parsimonia).\n")
}

# =============================================================================
# 4. MODELADO: LDA (LINEAR DISCRIMINANT ANALYSIS)
# =============================================================================

# Ajustamos LDA. CV=TRUE realiza Leave-One-Out Cross Validation automáticamente
modelo_lda <- lda(cyl ~ mpg + disp, data = df_da, CV = TRUE) 

# Matriz de Confusión (LDA)
ct_lda <- table(Real = df_da$cyl, Predicho = modelo_lda$class)
acc_lda <- sum(diag(ct_lda)) / sum(ct_lda)

print(ct_lda)
cat(paste("Accuracy LDA:", round(acc_lda, 4), "\n"))


# =============================================================================
# 5. MODELADO: QDA (QUADRATIC DISCRIMINANT ANALYSIS)
# =============================================================================

# Ajustamos QDA
modelo_qda <- qda(cyl ~ mpg + disp, data = df_da, CV = TRUE)

# Matriz de Confusión (QDA)
ct_qda <- table(Real = df_da$cyl, Predicho = modelo_qda$class)
acc_qda <- sum(diag(ct_qda)) / sum(ct_qda)

print(ct_qda)
cat(paste("Accuracy QDA:", round(acc_qda, 4), "\n"))


# =============================================================================
# 6. VISUALIZACIÓN DE FRONTERAS DE DECISIÓN (GRID PREDICTION)
# =============================================================================
# Definir rango del grid
rango_mpg  <- seq(min(df_da$mpg)-1, max(df_da$mpg)+1, length.out = 200)
rango_disp <- seq(min(df_da$disp)-10, max(df_da$disp)+10, length.out = 200)
grid_pred  <- expand.grid(mpg = rango_mpg, disp = rango_disp)

# Entrenamos modelos finales (sin CV) para proyectar en el grid
lda_final <- lda(cyl ~ mpg + disp, data = df_da)
qda_final <- qda(cyl ~ mpg + disp, data = df_da)

# Predecimos sobre el grid
pred_grid_lda <- predict(lda_final, grid_pred)$class
pred_grid_qda <- predict(qda_final, grid_pred)$class

grid_viz <- grid_pred %>%
  mutate(LDA_Class = pred_grid_lda,
         QDA_Class = pred_grid_qda)

# GRÁFICO 1: FRONTERAS LDA (LINEALES)
p1 <- ggplot() +
  # Fondo clasificado
  geom_raster(data = grid_viz, aes(x = mpg, y = disp, fill = LDA_Class), alpha = 0.3) +
  # Puntos reales
  geom_point(data = df_da, aes(x = mpg, y = disp, color = cyl), size = 3, shape = 21, stroke = 1) +
  # Líneas de contorno (opcional para ver bordes claros)
  stat_contour(data = grid_viz, aes(x = mpg, y = disp, z = as.numeric(LDA_Class)), 
               breaks = c(1.5, 2.5), color = "black", size = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Fronteras LDA (Linear)", subtitle = "Supuesto: Covarianzas Iguales", fill="Predicción") +
  theme_minimal()

# GRÁFICO 2: FRONTERAS QDA (CUADRÁTICAS)
p2 <- ggplot() +
  geom_raster(data = grid_viz, aes(x = mpg, y = disp, fill = QDA_Class), alpha = 0.3) +
  geom_point(data = df_da, aes(x = mpg, y = disp, color = cyl), size = 3, shape = 21, stroke = 1) +
  stat_contour(data = grid_viz, aes(x = mpg, y = disp, z = as.numeric(QDA_Class)), 
               breaks = c(1.5, 2.5), color = "black", size = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Fronteras QDA (Quadratic)", subtitle = "Supuesto: Covarianzas Diferentes", fill="Predicción") +
  theme_minimal()

# Mostrar gráficos
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
