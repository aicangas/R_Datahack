# Carga de librerías y de datos necesarios.
library(readxl)
library(xlsx)
library(stats)
attach(tabla)
# Lectura del fichero Excel que es fuente de datos. 
tabla<-read_excel("C:/Users/C09955A/Desktop/Data20170125/t.test_04.xlsx")
tabla

# Hipótesis nula: la distribución es normal.
shapiro.test(tabla$CHICOS)
# p-valor>0,05. Aceptamos la hipótesis nula, la distribución es normal.

# Hipótesis nula: la distribución es normal.
shapiro.test(tabla$CHICAS)
# p-valor>0,05. Aceptamos la hipótesis nula, la distribución es normal.

# H0: las varianzas son iguales.
var.test(tabla$CHICOS, tabla$CHICAS)
# p-valor>0,05. Aceptamos H0, las varianzas son iguales.

# H0: las medias son iguales.
t.test(tabla$CHICOS, tabla$CHICAS, var.equal=T, paired=T)
# p-valor>0,05. Aceptamos H0, las medias son iguales.
