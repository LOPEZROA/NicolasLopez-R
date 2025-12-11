# =============================================================================
# REGRESIÓN LOGÍSTICA BINARIA (GLM)
# =============================================================================

# 1. LIBRERÍAS
install.packages(c("tidyverse","caret","pROC","car","pscl"))
library(tidyverse)
library(caret)      
library(pROC)       
library(car)        
library(pscl)       

data("mtcars")

# 2. PREPARACIÓN Y AED

df_log <- mtcars %>%
  select(am, wt, hp, qsec) %>% 
  mutate(am = factor(am, levels = c(0, 1), labels = c("Automatico", "Manual")))

# A. Visualización de la Separación de Clases

p1 <- ggplot(df_log, aes(x = am, y = wt, fill = am)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(title = "Separación de Clases: Peso vs Transmisión",
       subtitle = "Los manuales tienden a ser más ligeros",
       y = "Peso (1000 lbs)") +
  theme_minimal()
print(p1)

# B. Tabla de Contingencia base (si tuviéramos variables categóricas)
print(table(df_log$am))


# =============================================================================
# 3. AJUSTE DEL MODELO (GLM BINOMIAL)
# =============================================================================
modelo_logit <- glm(am ~ wt + hp, family = "binomial", data = df_log)

summary_logit <- summary(modelo_logit)
print(summary_logit)

# =============================================================================
# 4. INTERPRETACIÓN ESTADÍSTICA (ODDS RATIOS)
# =============================================================================

or_table <- exp(cbind(OR = coef(modelo_logit), confint(modelo_logit)))
print(round(or_table, 4))

# =============================================================================
# 5. DIAGNÓSTICO Y BONDAD DE AJUSTE
# =============================================================================
# A. Multicolinealidad (VIF)
print(vif(modelo_logit))

# B. Pseudo R-Cuadrado (McFadden)
# Valores entre 0.2 y 0.4 indican un ajuste excelente.
r2_mcfadden <- pR2(modelo_logit)["McFadden"]
print(r2_mcfadden)

# =============================================================================
# 6. EVALUACIÓN DE PREDICCIÓN (CLASIFICACIÓN)
# =============================================================================

# A. Probabilidades Predichas
probabilidades <- predict(modelo_logit, type = "response")

# B. Curva ROC y AUC (Area Under Curve)
roc_obj <- roc(df_log$am, probabilidades)
auc_val <- auc(roc_obj)

cat(paste("AUC (Area Under Curve):", round(auc_val, 4), "\n"))
if(auc_val > 0.9) cat("--> Discriminación Sobresaliente.\n")

# Graficamos ROC
plot(roc_obj, main = paste("Curva ROC (AUC =", round(auc_val, 3), ")"),
     col = "darkred", lwd = 2)

# C. Matriz de Confusión (Umbral de corte = 0.5)
clase_predicha <- ifelse(probabilidades > 0.5, "Manual", "Automatico")
clase_predicha <- factor(clase_predicha, levels = c("Automatico", "Manual"))

cm <- confusionMatrix(clase_predicha, df_log$am)
print(cm$table)
cat("\nAccuracy (Exactitud Global):", round(cm$overall['Accuracy'], 3), "\n")


# =============================================================================
# 7. VISUALIZACIÓN DE LA CURVA SIGMOIDE
# =============================================================================
ggplot(df_log, aes(x = wt, y = as.numeric(am) - 1)) +
  geom_point(alpha = 0.5, size = 3, aes(color = am)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), 
              color = "black", se = TRUE) +
  labs(title = "Regresión Logística: Probabilidad de ser Manual según Peso",
       y = "Probabilidad (P(am=Manual))",
       x = "Peso (1000 lbs)",
       color = "Transmisión Real") +
  theme_minimal()
