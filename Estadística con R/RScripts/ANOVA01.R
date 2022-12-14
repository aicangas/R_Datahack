# Carga de librer?as y de datos necesarios.
library(readxl)
library(xlsx)
library(stats)

# Lectura del fichero Excel que es fuente de datos. 
tabla<-read_excel("C:/Users/ivang/Desktop/M?ster Datahack - Estad?stica/04.- Datahack_ Estad?stica - Mayo 2017/00.- Clases/ANOVA01.xlsx")
tabla

m1<-aov(NOTA~CENTRO, data=tabla)
summary(m1)

# Como el p-valor del test F de Fisher es menor que 0,05, existen diferencias significativas entre las 
# calificaciones de los diferentes centros al 95% de confianza, con lo que admitimos que existe variabilidad 
# entre centros.

# Medias.
tapply(tabla$NOTA,tabla$CENTRO,mean)

# Desviaciones t?picas.
tapply(tabla$NOTA,tabla$CENTRO,sd)

# Diagramas de cajas.
boxplot(tabla$NOTA~tabla$CENTRO)

# M?todo de Tukey.
Tukey<-TukeyHSD(m1,"CENTRO")
Tukey
# Las diferencias m?s significativas se dan entre los centros 2 y 4.

plot(Tukey)
