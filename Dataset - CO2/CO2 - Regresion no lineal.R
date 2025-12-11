# =============================================================================
# REGRESIÓN NO LINEAL (NLS) + DIAGNÓSTICO
# =============================================================================
install.packages(c("lmtest", "nlme"))
library(lmtest) 


# Definición de la función teórica
mm_model <- function(conc, Vmax, K) { (Vmax * conc) / (K + conc) }

# Ajuste global 
nls_fit <- nls(uptake ~ mm_model(conc, Vmax, K), 
               data = CO2, 
               start = list(Vmax = 30, K = 50))

print(summary(nls_fit))

# --- DIAGNÓSTICO DE SUPUESTOS ---

res_nls <- resid(nls_fit)
fit_nls <- fitted(nls_fit)

# 1. Normalidad (Shapiro)
print(shapiro.test(res_nls))

# 2. Homocedasticidad (Breusch-Pagan modificado o Visual)
par(mfrow = c(1, 3))
plot(fit_nls, res_nls, main = "Residuos vs Ajustados", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# 3. INDEPENDENCIA (El supuesto crítico en series/repetidas)
acf(res_nls, main = "ACF de Residuos (Autocorrelación)")

# 4. Q-Q Plot
qqnorm(res_nls, main = "Q-Q Plot NLS"); qqline(res_nls)
par(mfrow = c(1, 1))

# Test de Durbin-Watson (Adaptación manual o visual)

# --- SOLUCIÓN AVANZADA ---

# Implementación rápida de nlme para contrastar
library(nlme)
nlme_fit <- nlme(uptake ~ mm_model(conc, Vmax, K),
                 data = CO2,
                 fixed = Vmax + K ~ 1,
                 random = Vmax ~ 1 | Plant, 
                 start = c(Vmax = 30, K = 50))
print(summary(nlme_fit))
