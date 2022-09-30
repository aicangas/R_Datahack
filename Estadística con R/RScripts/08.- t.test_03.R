# Carga de librerías y de datos necesarios.
library(readxl)
library(xlsx)
library(stats)

# Lectura del fichero Excel que es fuente de datos. 
tabla<-read_excel("C:/Users/ivang/Desktop/Máster Datahack - Estadística/04.- Datahack_ Estadística - Mayo 2017/00.- Clases/t.test_03.xlsx")
tabla

# Hipótesis nula: la distribución es normal.
shapiro.test(tabla$X)
# p-valor>0,05. Aceptamos la hipótesis nula, la distribución es normal.

# Hipótesis nula: la distribución es normal.
shapiro.test(tabla$Y)
# p-valor>0,05. Aceptamos la hipótesis nula, la distribución es normal.

# H0: las varianzas son iguales.
var.test(tabla$X, tabla$Y)
# p-valor>0,05. Aceptamos H0, las varianzas son iguales.

# H0: las medias son iguales.
t.test(tabla$X, tabla$Y, var.equal=T)
# p-valor>0,05. Aceptamos H0, las medias son iguales.


# H0: la media de X es menor o igual que la media de Y.
# Hipótesis alternativa: mux>muy
t.test(tabla$X, tabla$Y, var.equal=T, alternative = "greater", paired = T)
# p-valor>0,05. Aceptamos H0, el verapamil es peor o igual que el nitropusside.






