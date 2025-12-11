# =============================================================================
# ANÁLISIS DE COMPONENTES PRINCIPALES (PCA) 
# =============================================================================

# 1. LIBRERÍAS
install.packages(c("tidyverse","factoextra","corrplot","psych"))
library(tidyverse)
library(factoextra) 
library(corrplot)   
library(psych)      

data("mtcars")

# 2. PREPARACIÓN DE DATOS Y AED
df_pca <- mtcars %>% 
  select(-vs, -am)


# A. Matriz de Correlación
matriz_cor <- cor(df_pca)
corrplot(matriz_cor, method = "color", type = "upper", 
         tl.col = "black", addCoef.col = "black", number.cex = 0.7,
         title = "Matriz de Correlación: Justificación del PCA", mar=c(0,0,2,0))

# B. Test de Esfericidad de Bartlett
# H0: La matriz de correlación es una matriz identidad (Variables no correlacionadas).
# H1: Las variables están correlacionadas (PCA es adecuado).
bartlett <- cortest.bartlett(matriz_cor, n = nrow(df_pca))

# C. Medida Kaiser-Meyer-Olkin (KMO)
# Mide la adecuación muestral. > 0.6 es aceptable, > 0.8 es excelente.
kmo_res <- KMO(df_pca)

cat(paste("Bartlett p-value:", format(bartlett$p.value, digits = 4), "\n"))
cat(paste("KMO Global:", round(kmo_res$MSA, 2), "\n"))

if(bartlett$p.value < 0.05 & kmo_res$MSA > 0.6) {
  cat("[CONCLUSIÓN]: Datos adecuados para reducción de dimensionalidad.\n")
} else {
  cat("[ALERTA]: Los datos podrían no ser adecuados para PCA.\n")
}


# =============================================================================
# 3. CÁLCULO DEL PCA
# =============================================================================

res_pca <- prcomp(df_pca, scale = TRUE)

# Resumen de varianza explicada
summary(res_pca)

# =============================================================================
# 4. SELECCIÓN DE COMPONENTES (CRITERIOS)
# =============================================================================

# Gráfico de Sedimentación (Scree Plot)
fviz_eig(res_pca, addlabels = TRUE, ylim = c(0, 70), 
         barfill = "steelblue", barcolor = "steelblue",
         main = "Scree Plot: Varianza Explicada por Dimensión")

# Extraemos Autovalores (Eigenvalues)
eig_val <- get_eigenvalue(res_pca)
print(head(eig_val))

# =============================================================================
# 5. INTERPRETACIÓN DE LOS COMPONENTES (VARIABLES)
# =============================================================================
# Círculo de Correlaciones
fviz_pca_var(res_pca, col.var = "contrib", 
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE) +
  labs(title = "Círculo de Correlaciones", 
       subtitle = "Rojo = Variable muy importante para definir los ejes")

# Contribución numérica a las dimensiones 1 y 2
print(res_pca$rotation[, 1:2])


# =============================================================================
# 6. BIPLOT: EL MAPA FINAL
# =============================================================================

# Proyectamos los autos en el nuevo espacio de 2 dimensiones.
fviz_pca_biplot(res_pca, 
                geom.ind = "point",
                pointshape = 21,
                pointsize = 3,
                fill.ind = factor(mtcars$am), # Colorear por Transmisión (Manual/Auto)
                col.ind = "black",
                palette = "jco", 
                addEllipses = TRUE, # Elipses de concentración
                label = "var",      # Solo etiquetar variables (para no saturar)
                
                # Configuración Variables
                col.var = "grey40",
                repel = TRUE,
                
                title = "PCA Biplot: Mapa del Mercado Automotriz (1974)",
                legend.title = "Transmisión\n(0=Auto, 1=Man)"
)
