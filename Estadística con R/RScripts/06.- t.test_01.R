# Carga de librerías y de datos necesarios.
library(readxl)
library(xlsx)
library(stats)

# Lectura del fichero Excel que es fuente de datos. 
tabla<-read_excel("C:/Users/ivang/Desktop/Máster Datahack - Estadística/05.- Datahack_ Estadística - Octubre 2017/00.- Clases/t.test_01.xlsx")
tabla
attach(tabla)
t.test(tabla$longitud)

# Hipótesis nula: la distribución es normal.
shapiro.test(tabla$longitud)
# p-valor>0,05. Aceptamos la hipótesis nula, la distribución es normal.
