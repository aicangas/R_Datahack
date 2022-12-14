# Carga de librer?as y de datos necesarios.
library(readxl)
# library(xlsx)
library(stats)

options(scipen = 100)

# Lectura del fichero Excel que es fuente de datos. 
tabla<-read_excel("C:/Users/ivang/Desktop/M?ster Datahack - Estad?stica/08.- Datahack_ Estad?stica - Octubre 2018/Tablas/Regresi?n Lineal.xlsx")

# Visualizaci?n de la tabla de partida.
View(tabla)

# Resumen estad?stico de las variables.
summary(tabla)

# Nombres de las variables/columnas de la tabla.
names(tabla)

# Para conocer las relaciones existentes entre cada par de variables 
# representamos la matriz de diagramas de dispersi?n. 
pairs(tabla)

# Se aprecia una relaci?n lineal evidente entre las variables "edad" y "grasas".
# Entre en resto de pares de variables no se observa ninguna relaci?n lineal.

# Matriz de correlaciones entre los pares de variables de la tabla.
(cor(tabla))^2
cov(tabla)

# La correlaci?n m?s alta, y cercana a 1, se da entre grasas y edad.

# Regresi?n lineal.
regresion<-lm(grasas~edad, data=tabla)
summary(regresion)

# Los par?metros de la ecuaci?n de la recta de m?nimos cuadrados 
# que relaciona la cantidad de grasas en la sangre en funci?n del peso 
# vienen dados por la columna ?Estimate? de la tabla ?Coefficients? de la salida.

# La pendiente es 5,3207: Estimate-edad.
pendiente<-cov(tabla$edad,tabla$grasas)/var(tabla$edad)
pendiente

# El t?rmino independiente es 102,5751: Estimate-(Intercept)
independiente<-mean(tabla$grasas)-pendiente*mean(tabla$edad)
independiente

# La recta de regresi?n es: grasas=5,3207edad+102,5751.

# El coeficiente de determinaci?n (que mide la bondad del ajuste de la recta a los datos) 
# es Multiple R-squared: 0,701.
coeficiente_R2<-(cov(tabla$edad,tabla$grasas)/(sd(tabla$edad)*sd(tabla$grasas)))^2
coeficiente_R2
  
# Nube de puntos y recta de m?nimos cuadrados.
plot(tabla$edad, tabla$grasas, xlab="Edad", ylab="Grasas")
abline(regresion)

# Predicciones de grasas mediante la recta de regresi?n para edades comprendidas entre 30 y 50 a?os.
(nuevas.edades<-data.frame(edad=seq(30, 50)))
predict(regresion, nuevas.edades)

# Para alguien de 30 a?os predecimos una cantidad de grasas de 262.1954.
# Para alguien de 31 a?os predecimos una cantidad de grasas de 267,5161.
# ...
# ...
# Para alguien de 50 a?os predecimos una cantidad de grasas de 368,6030.

# Diagn?stico del modelo.

(residuos<- rstandard(regresion))
(valores.ajustados<-fitted(regresion))
plot(valores.ajustados, residuos)
# No se observa ning?n patr?n especial: la homocedasticidad y la linealidad resultan hip?tesis razonables.

tabla$prediccion <- valores.ajustados
tabla$residuos_cuadrado <- (tabla$grasas - tabla$prediccion)^2

(cv_res_cuad <- sd(tabla$residuos_cuadrado) / mean(tabla$residuos_cuadrado))
(tabla$residuos_reales <- (tabla$grasas - tabla$prediccion))
(tabla$residuos_reales_abs <- abs(tabla$grasas - tabla$prediccion))


qqnorm(residuos)
qqline(residuos)
# Los puntos est?n bastante alineados, la normalidad tambi?n parece aceptable.

library(e1071)
skewness(residuos)
kurtosis(residuos)
shapiro.test(residuos)


plot(regresion)






