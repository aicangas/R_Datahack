# Carga de librerías y de datos necesarios.
library(readxl)
library(xlsx)
library(stats)
# Lectura del fichero Excel que es fuente de datos. 
tabla<-read_excel("C:/Users/ivang/Desktop/Machine Learning & Math Backup/Datahack/BCDS MAY2020/Tablas/Regresión Lineal Múltiple.xlsx")
attach(tabla)

regresion_multiple <- lm(estancia ~ admisiones + edad)
regresion_multiple
summary((regresion_multiple))

# PRIMER MODELO: ESTANCIA = 0.05699 * ADMISIONES + 1.05002 * EDAD + 2.08572

# H0: las admisiones no contribuyen a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
# Probabilidad de tener los datos del problema bajo esa hipótesis: p-value 0.98296.
# p - value por debajo de 0.05 nos acredita a rechazar H0. No es nuestro caso, el p-valor es 0.98296 > 0.05.
# Por tanto, no nos vemos obligados a rechazar la hipótesis inicial H0.
# Es decir, concluimos que las admisiones no contribuyen a la predicción en el modelo, por lo que podemos extraer esa variable de la ecuación.

regresion_multiple_2<-lm(estancia ~ edad)
regresion_multiple_2
summary(regresion_multiple_2)

# H0: la edad no contribuye a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
# Probabilidad de tener los datos del problema bajo esa hipótesis: p-value 0.00000101.
# p - value por debajo de 0.05 nos acredita a rechazar H0. Es nuestro caso, el p-valor es 0.00000101 < 0.05.
# Por tanto, nos vemos obligados a rechazar la hipótesis inicial H0.
# Es decir, concluimos que la edad sí contribuye a la predicción en el modelo, por lo que no podemos extraer esa variable de la ecuación.

# SEGUNDO MODELO: ESTANCIA = 1.05 * EDAD + 1.977

regresion_multiple_3<-lm(estancia ~ edad - 1)
regresion_multiple_3
summary(regresion_multiple_3)

# TERCER MODELO: ESTANCIA = 1.10877 * EDAD

plot(regresion_multiple_2)

residuos<-fitted(regresion_multiple_2)-tabla$estancia
skewness(residuos)
kurtosis(residuos)
boxplot.stats(residuos)

regresion_multiple_4<-lm(estancia ~ admisiones * edad)
regresion_multiple_4
summary((regresion_multiple_4))

# CUARTO MODELO: ESTANCIA = 7.91130 * ADMISIONES + 1.73358 * EDAD - 0.2431 * ADMIOSIONES * EDAD - 15.88012

regresion_multiple_5<-lm(estancia~admisiones*edad-1)
regresion_multiple_5
summary((regresion_multiple_5))


qqnorm(rstandard(regresion_multiple_2))
qqline(rstandard(regresion_multiple_2))
plot(regresion_multiple_2)


