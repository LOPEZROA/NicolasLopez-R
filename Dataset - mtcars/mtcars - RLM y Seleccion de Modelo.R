# =============================================================================
# REGRESIÓN LINEAL MÚLTIPLE Y SELECCIÓN DE MODELOS 
# =============================================================================
install.packages(c("tidyverse","car","corrplot","MASS","lmtest"))

library(tidyverse)
library(car)        
library(corrplot)   
library(MASS)       
library(lmtest)     

data("mtcars")

# PRE-PROCESAMIENTO Y AED
# Convertimos variables categóricas binarias a factores para un análisis correcto
df_autos <- mtcars %>%
  mutate(
    vs = as.factor(vs), 
    am = as.factor(am)  
  )


# Distribución de la variable respuesta (mpg)
hist(df_autos$mpg, main = "Distribución de Millas por Galón (mpg)", 
     xlab = "mpg", col = "steelblue", border = "white", breaks = 10)

# =============================================================================
# AJUSTE DEL MODELO COMPLETO (FULL MODEL)
# =============================================================================

modelo_full <- lm(mpg ~ ., data = df_autos)
summary_full <- summary(modelo_full)

print(summary_full)

# DIAGNÓSTICO DE MULTICOLINEALIDAD (VIF)
vif_values <- car::vif(modelo_full)
print(vif_values)

# =============================================================================
# SELECCIÓN DE MODELOS (STEPWISE SELECTION)
# =============================================================================

# Usamos stepAIC en dirección "both" (pone y saca variables)
# El criterio es minimizar el AIC (Akaike Information Criterion)
modelo_step <- stepAIC(modelo_full, direction = "both", trace = FALSE)

print(summary(modelo_step))


# =============================================================================
# DIAGNÓSTICO DE SUPUESTOS DEL MODELO FINAL
# =============================================================================

residuos_final <- residuals(modelo_step)

# A. NORMALIDAD DE RESIDUOS (Shapiro-Wilk)
# H0: Los residuos son Normales.
shapiro_test <- shapiro.test(residuos_final)
print(shapiro_test)

# B. HOMOCEDASTICIDAD (Breusch-Pagan)
# H0: La varianza del error es constante.
bp_test <- bptest(modelo_step)
print(bp_test)

# C. INDEPENDENCIA (Durbin-Watson)
dw_test <- durbinWatsonTest(modelo_step)
print(dw_test)

# D. MULTICOLINEALIDAD RESIDUAL
# Verificamos si el modelo final limpió el problema de VIF
print(car::vif(modelo_step))


# =============================================================================
# GRÁFICOS DE DIAGNÓSTICO
# =============================================================================
par(mfrow = c(2, 2)) 
plot(modelo_step)
par(mfrow = c(1, 1))

# =============================================================================
# IDENTIFICACIÓN DE OUTLIERS E INFLUYENTES
# =============================================================================
# Distancia de Cook: ¿Qué autos mueven drásticamente el modelo si los quitamos?
cooks_d <- cooks.distance(modelo_step)
umbral_cook <- 4 / nrow(df_autos) 

influyentes <- names(cooks_d)[cooks_d > umbral_cook]

if(length(influyentes) > 0) {
  cat(paste("Autos influyentes detectados (Cook's D > 4/n):", paste(influyentes, collapse = ", "), "\n"))
  cat("Nota: Evaluar si estos modelos son 'especiales' (ej. deportivos de lujo) y distorsionan la predicción.\n")
} else {
  cat("No se detectaron puntos excesivamente influyentes.\n")
}
