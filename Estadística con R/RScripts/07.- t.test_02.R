# Carga de librerías y de datos necesarios.
library(readxl)
library(xlsx)
library(stats)

# Lectura del fichero Excel que es fuente de datos. 
tabla<-read_excel("C:/Users/ivang/Desktop/Máster Datahack - Estadística/03.- Datahack_ Estadística - Enero 2017/t.test_02.xlsx")
tabla

# H0: las varianzas son iguales.
var.test(tabla$GRUPO_1, tabla$GRUPO_2)
# p-valor>0,05. Aceptamos H0, las varianzas son iguales.


# H0: las medias son iguales.
t.test(tabla$GRUPO_1, tabla$GRUPO_2, var.equal=T)
# p-valor<0,05. Rechazamos H0, las medias no son iguales.



shapiro.test(tabla$GRUPO_1)
shapiro.test(tabla$GRUPO_2)
