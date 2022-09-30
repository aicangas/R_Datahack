# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Carga de paquetes/librerías necesarias.
function_install_packages <- function(x)
{
  for(i in x)
  {
    if(!require(i, character.only = TRUE))
    {
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
}

function_install_packages(c("dplyr", "ggplot2", "corrplot", "ggcorrplot", "e1071", "GGally", "tidyverse", 
                            "ggpubr", "base", "car", "MASS", "leaps", "hier.part", "gvlma", "lmtest", "performance"))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Importación de los datos (con selección de las 5 variables necesarias para resolver este ejercicio).
DATOS <- as.data.frame(state.x77[ , c("Murder", "Population", "Illiteracy", "Income", "Frost")])

# Layout de variables.
# Murder:     ratio de asesinatos y homicidios involuntarios por cada 100.000 habitantes.
# Population: población del estado.
# Illiteracy: porcentaje de población analfabeta.
# Income:     ingresos per cápita.
# Frost:      número medio de días con temperatura por debajo del nivel de congelación.

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Correlaciones entre parejas de variables.
cor(DATOS)
ggcorrplot(cor(DATOS, use = "complete.obs"), hc.order = TRUE, type = "lower", lab = TRUE)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Creación de la variable "ESTADO" (denominación del estado en función de lo indicado por el nombre de la fila de la tabla).
DATOS$ESTADO <- row.names(DATOS)

# Creación de la variable que indica la división (NORTHEAST, MIDWEST, SOUTH, WEST) a la que pertenece cada uno de los estados.
DATOS$DIVISION <- ifelse(DATOS$ESTADO %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", 
                                             "Vermont", "New Jersey", "New York", "Pennsylvania"),                                       "NORTHEAST", 
                         
                  ifelse(DATOS$ESTADO %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", 
                                             "Missouri", "Nebraska", "North Dakota", "South Dakota"),                                    "MIDWEST", 
                         
                  ifelse(DATOS$ESTADO %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", 
                                             "Virginia", "District of Columbia", "West Virginia", "Alabama", "Kentucky", 
                                             "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"),                  "SOUTH", 
                  
                  ifelse(DATOS$ESTADO %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", 
                                             "Alaska", "California", "Hawaii", "Oregon", "Washington"),                                  "WEST", 
                         
                                                                                                                                         "NULL"))))

table(DATOS$DIVISION, useNA = "always")       # Comprobamos que cada estado ha quedado asignado a una única división.
sum(table(DATOS$DIVISION)) == nrow(DATOS)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Análisis avanzado de Estadística Descriptiva y Correlaciones entre variables (desglose según división).
ggpairs(DATOS[ , c(1:5, 7)], aes(colour = DATOS$DIVISION, alpha = 0.5), upper = list(continuous = wrap("cor", alignPercent = 0.5)))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Regresiones múltiples lineales que relacionan la variable de asesinatos con la población, el analfabetismo, los ingresos y los días con congelamiento.

# H0: la variable no contribuye a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
REGRESION_MULTIPLE_1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = DATOS)
summary(REGRESION_MULTIPLE_1)

REGRESION_MULTIPLE_2 <- lm(Murder ~ Population + Illiteracy + Income, data = DATOS)
summary(REGRESION_MULTIPLE_2)

REGRESION_MULTIPLE_3 <- lm(Murder ~ Population + Illiteracy, data = DATOS)
summary(REGRESION_MULTIPLE_3)

REGRESION_MULTIPLE_4 <- lm(Murder ~ Population + Illiteracy + DIVISION, data = DATOS)
summary(REGRESION_MULTIPLE_4)

# MURDER[MIDWEST]   = 0.00029188 * POPULATION + 2.75488017 * ILLITERACY + 1.94468281
# MURDER[NORTHEAST] = 0.00029188 * POPULATION + 2.75488017 * ILLITERACY + 0.3634274
# MURDER[SOUTH]     = 0.00029188 * POPULATION + 2.75488017 * ILLITERACY + 4.566377
# MURDER[WEST]      = 0.00029188 * POPULATION + 2.75488017 * ILLITERACY + 3.546009

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Proceso step que sintetiza la información obtenida en los pasos anteriores (seleccionando vía p-valor).
step(REGRESION_MULTIPLE_1, direction = "backward")  

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Intervalos de confianza al 95% para los coeficientes de las variables predictoras más importantes.
confint(lm(Murder ~ Population + Illiteracy, data = DATOS))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Resumen de la evolución sufrida por el R-cuadrado ajustado.
REGRESION_SALTOS <-regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data = DATOS, nbest = 4)
plot(REGRESION_SALTOS, scale = "adjr2")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Importancia relativa de cada variable (de entre las más representativas): aportación al R-cuadrado ajustado final.
(relat.imp.RMSPE = hier.part(DATOS$Murder, DATOS[ , c("Population", "Illiteracy")], family = "gaussian", gof = "RMSPE", barplot = TRUE))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Diagnóstico del modelo: homocedasticidad -vs- heterocedasticidad, normalidad de residuos y valores anómalos en la predicción.
par(mfrow = c(2,2))
plot(REGRESION_MULTIPLE_3)

# Test de Breusch-Pagan.
bptest(REGRESION_MULTIPLE_3)                    # p-valor > 0.05: no se rechaza la hipótesis nula de homocedasticidad. MODELO HOMOCEDÁSTICO.

# Test de Shapiro-Wilk.
shapiro.test(REGRESION_MULTIPLE_3$residuals)    # p-valor > 0.05: no se rechaza la hipótesis nula de normalidad. RESIDUOS CON DISTRIBUCIÓN NORMAL.
qqPlot(REGRESION_MULTIPLE_3, labels = row.names(DATOS), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")

# Test de Durbin-Watson.
dwtest(REGRESION_MULTIPLE_3)                    # p-valor > 0.05: no se rechaza la hipótesis nula de no existencia de autocorrelación: RESIDUOS INDEPENDIENTES.

gvlma(REGRESION_MULTIPLE_3)         # Cumplimiento de las condiciones del modelo.

outlierTest(REGRESION_MULTIPLE_3)   # Outliers.

vif(REGRESION_MULTIPLE_3)           # Valores por debajo de 5 e incluso cercanos a 1: AUSENCIA DE COLINEALIDAD.

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

check_model(REGRESION_MULTIPLE_3)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------



# MURDER = A * POPULATION + 1.94468281



