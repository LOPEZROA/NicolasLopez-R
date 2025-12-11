# =============================================================================
# MODELO MIXTO (LMM) 
# =============================================================================
install.packages(c("lme4","lmerTest","car","performance"))
library(lme4)
library(lmerTest) 
library(car)      
library(performance) 


model_lmm <- lmer(uptake ~ Type * Treatment + log(conc) + (1 | Plant), 
                  data = CO2)

print(anova(model_lmm))

# --- DIAGNÓSTICO DE SUPUESTOS ---

# 1. Extracción de Residuos y Efectos Aleatorios
residuos <- resid(model_lmm)
ajustados <- fitted(model_lmm)
rand_eff <- ranef(model_lmm)$Plant$`(Intercept)`

# 2. Supuesto de Normalidad de los Residuos (Shapiro-Wilk)
shapiro_res <- shapiro.test(residuos)
print(shapiro_res)
# Interpretación: Si p > 0.05, asumimos normalidad.

# 3. Supuesto de Homocedasticidad (Residuos vs Ajustados)
par(mfrow = c(2, 2))
plot(model_lmm) 
qqnorm(residuos, main = "Q-Q Residuos"); qqline(residuos)

# 4. Supuesto de Normalidad de Efectos Aleatorios (Interceptos por planta)
qqnorm(rand_eff, main = "Q-Q Efectos Aleatorios (Plant)"); qqline(rand_eff)
shapiro_rand <- shapiro.test(rand_eff)
title(sub = paste("Shapiro p =", round(shapiro_rand$p.value, 3)))

# 5. Visualización de Ajuste
plot(CO2$uptake, ajustados, main = "Observed vs Predicted", 
     xlab = "Real", ylab = "Predicho")
abline(0, 1, col = "red")
par(mfrow = c(1, 1))

