######################################################################################################################
#                                                                                                                    #
# CONTENIDO.                                                                                                         #
#                                                                                                                    #
# INSTALACIÃN DE PAQUETES Y CARGAS DE LIBRERÃAS.                                                                     #
# IMPORTACIÃN DE TABLAS DE DATOS.                                                                                    #
# ENCRIPTACIÃN DE INFORMACIÃN.                                                                                       #
# AUDITORÃA DE DATOS:                                                                                                #
#   - FUNCIONES, BUCLES Y CONDICIONES: DETECCIONES DE NULOS y DE DUPLICADOS.                                         #
#   - CORRELACIONES LINEALES.                                                                                        #
#   - TABLAS CRUZADAS: FRECUENCIAS ESPERADAS Y PRUEBAS DE INDEPENDENCIA ENTRE VARIABLES.                             #
#   - FILTROS Y AGREGACIONES.                                                                                        #
# DIAGRAMAS DE CAJA Y GRÃFICOS AVANZADOS. TESTS DE NORMALIDAD DE DISTRIBUCIONES.                                     #
# INTERACCIÃN ENTRE VARIABLES, ANÃLISIS DE VARIANZA Y COMPARACIONES MÃLTIPLES.                                       #
# REGRESIÃN LOGÃSTICA: DESARROLLO Y VALIDACIÃN.                                                                      #
#                                                                                                                    #
######################################################################################################################

# Borrado de todas las variables del workspace #######################################################################
rm(list=ls())

# InstalaciÃ³n de paquetes (sÃ³lo en caso de no estar instalados previamente):
if (!require(WRS2)){ 
  install.packages("WRS2") 
}
if (!require(readxl)){ 
  install.packages("readxl") 
}
if (!require(stringi)){ 
  install.packages("stringi") 
} 
if (!require(digest)){ 
  install.packages("digest") 
}
if (!require(VIM)){ 
  install.packages("VIM") 
}
if (!require(ggcorrplot)){ 
  install.packages("ggcorrplot") 
} 
if (!require(dplyr)){ 
  install.packages("dplyr") 
}


# Carga de librerÃ­as necesarias.
library(WRS2)
library(readxl)
library(stringi)
library(digest)
library(VIM)
library(ggcorrplot)
library(dplyr)

# Lectura de la tabla global #########################################################################################
TABLA_GLOBAL<-read_excel("C:/Users/ivang/Desktop/Machine Learning & Math Backup/Datahack/BCDS MAY2020/Tablas/Tabla.xlsx")
str(TABLA_GLOBAL)

# EncriptaciÃ³n de la informaciÃ³n #####################################################################################
# CreaciÃ³n de documentos aleatorios.
TABLA_GLOBAL$DOCUMENTOS<-(sprintf("%s%s", stri_rand_strings(51, 8, '[0-9]'), stri_rand_strings(51, 1, '[A-Z]')))
names(TABLA_GLOBAL)

# Nueva variable con los documentos encriptados mediante sha1.
TABLA_GLOBAL$DOCUMENTOS_ENCRIPTADOS_SHA1<-sapply(TABLA_GLOBAL$DOCUMENTOS, sha1)

# Otras formas de encriptar:
TABLA_GLOBAL$DOCUMENTOS_ENCRIPTADOS_DIGEST_SHA1<-sapply(TABLA_GLOBAL$DOCUMENTOS, digest, algo="sha1")
TABLA_GLOBAL$DOCUMENTOS_ENCRIPTADOS_DIGEST_MD5<-sapply(TABLA_GLOBAL$DOCUMENTOS, digest, algo="md5")

# EliminaciÃ³n de las columnas que procedan ser descartadas:
TABLA_GLOBAL<-TABLA_GLOBAL[ , -c(10, 11, 12, 13)]

# AuditorÃ­a de datos #################################################################################################

aggr(TABLA_GLOBAL, numbers = TRUE)
summary(aggr(TABLA_GLOBAL, numbers = TRUE))

# DetecciÃ³n de columnas con al menos un registro con valor nulo:
sapply(TABLA_GLOBAL, function(x) all(!is.na(x)))

# FunciÃ³n para conocer si una variable concreta estÃ¡ totalmente documentada o si contiene nulos:
NA_COUNT<-function(x){
for(i in 1:length(x)){
   if(is.na(x[i])) {
      print("AlgÃºn valor de la variable es nulo")
      break}
}}

NA_COUNT(TABLA_GLOBAL$SEXO)

# DetecciÃ³n de duplicados:
nrow(TABLA_GLOBAL[duplicated(TABLA_GLOBAL$ID), ])     # 2 ID's duplicados.
nrow(TABLA_GLOBAL[!duplicated(TABLA_GLOBAL$ID), ])    # 49 ID's no duplicados (uno de ellos asociado a campos vacÃ­os).

# ContrucciÃ³n de la tabla final correctamente documentada.
TABLA_GLOBAL<-TABLA_GLOBAL[!duplicated(TABLA_GLOBAL$ID)&(!is.na(TABLA_GLOBAL$SEXO)), ]
aggr(TABLA_GLOBAL)

# Correlaciones lineales. ############################################################################################
pairs(TABLA_GLOBAL[ , c(3, 4, 5, 6, 8)])
cor(TABLA_GLOBAL[ , c(3, 4, 5, 6, 8)])
# La cantidad de copas tomadas estÃ¡ correlacionada positiva y fuertemente con el gasto en la noche; en menor medida
# lo estÃ¡, tambiÃ©n positivamente, con la tasa de alcohol.

# Mejor versiÃ³n de la tabla de correlaciones.
ggcorrplot(cor(TABLA_GLOBAL[ , c(3, 4, 5, 6, 8)], use="complete.obs"), hc.order=TRUE, type="lower", lab=TRUE)

# Tabla cruzada con distribuciones. ##################################################################################
table(TABLA_GLOBAL$CANTIDAD_COPAS, TABLA_GLOBAL$NACIONALIDAD)

# Frecuencias esperadas.
chisq.test(table(TABLA_GLOBAL$CANTIDAD_COPAS, TABLA_GLOBAL$NACIONALIDAD))$expected

# Prueba de independencia Chi-Cuadrado para determinar si la nacionalidad afecta a la cantidad de copas tomadas.
# HipÃ³tesis nula H0: ambas variables son independientes.
chisq.test(table(TABLA_GLOBAL$CANTIDAD_COPAS, TABLA_GLOBAL$NACIONALIDAD))
# p-valor=0,007576<0,05: rechazamos la hipÃ³tesis nula (de independencia entre variables). 
# Por tanto, existe una relaciÃ³n relevante entre la nacionalidad y la cantidad de copas tomadas.

# Prueba exacta de Fisher.
# HipÃ³tesis nula: las variables son independientes, no existe asociaciÃ³n entre nacionalidad y cantidad de copas.
fisher.test(table(TABLA_GLOBAL$CANTIDAD_COPAS, TABLA_GLOBAL$NACIONALIDAD))
# p-value=0,003973<0,05: rechazamos la hipÃ³tesis nula: existe relaciÃ³n entre la nacionalidad y la cantidad de copas 
# tomadas, la proporciÃ³n de britÃ¡nicos con cuatro copas es, en proporciÃ³n relevante, superior a la esperada.

# Filtros y Agregaciones. ############################################################################################
TABLA_CHICOS<-TABLA_GLOBAL[TABLA_GLOBAL$SEXO=="Chico", ]
TABLA_CHICAS<-TABLA_GLOBAL[TABLA_GLOBAL$SEXO=="Chica", ]

# CÃ¡lculos en funciÃ³n de algunas agregaciones:
(TASA_ALCOHOL_MEDIA_SEXO<-aggregate(TABLA_GLOBAL$TASA_ALCOHOL, list(TABLA_GLOBAL$SEXO), mean))
TABLA_GLOBAL %>% group_by(SEXO) %>% summarise(TASA_ALCOHOL_MEDIA=mean(TASA_ALCOHOL)) # Misma operaciÃ³n con dplyr.

TASA_ALCOHOL_MEDIA_NACIONALIDAD<-aggregate(TABLA_GLOBAL$TASA_ALCOHOL, list(TABLA_GLOBAL$NACIONALIDAD), mean)
EDAD_MEDIA_NACIONALIDAD<-aggregate(TABLA_GLOBAL$EDAD, list(TABLA_GLOBAL$NACIONALIDAD), mean)

# GrÃ¡ficos. ##########################################################################################################

# Diagramas de Caja segÃºn sexo, segÃºn cantidad de copas y segÃºn la interacciÃ³n entre estas dos variables.
boxplot(TABLA_GLOBAL$NOTA_ATRACTIVO_LIGUE~TABLA_GLOBAL$SEXO, data=TABLA_GLOBAL, las=2)
boxplot(TABLA_GLOBAL$NOTA_ATRACTIVO_LIGUE~TABLA_GLOBAL$CANTIDAD_COPAS, data=TABLA_GLOBAL, las=2)
boxplot(TABLA_GLOBAL$NOTA_ATRACTIVO_LIGUE~TABLA_GLOBAL$SEXO*TABLA_GLOBAL$CANTIDAD_COPAS, data=TABLA_GLOBAL, las=2)

# Boxplots avanzados. ################################################################################################
boxplot_avanzado<-function(x,y)
{
  stats=boxplot.stats(x)$stats
  f=fivenum(x)
  stats2<-c(f[1],stats,f[5])
  stats3<-c(f[1],f[5])
  
  boxplot(x,main=y,col="steelblue3")
  abline(h=stats[1],lty=2,col="red")
  abline(h=stats[5],lty=2,col="red")
  text(rep(1.35,5),stats,labels=c('BIGOTE INFERIOR','PRIMER CUARTIL','MEDIANA','TERCER CUARTIL','BIGOTE SUPERIOR'))
  text(rep(.5,7),stats2,labels=round(stats2,digits=4),cex=0.6)
  text(rep(0.75,2),stats3,labels=c('MÃNIMO','MÃXIMO'))
}
# Visualizamos los diagramas de caja para una de las variables.
boxplot_avanzado(TABLA_CHICOS$TASA_ALCOHOL, 'Tasa de Alcohol en los Chicos')
boxplot_avanzado(TABLA_CHICAS$TASA_ALCOHOL, 'Tasa de Alcohol en las Chicas')

# Tests de normalidad de distribuciones de las variables. ############################################################
# HipÃ³tesis nula: la tasa de alcohol sigue una distribuciÃ³n normal.
shapiro.test(TABLA_CHICOS$TASA_ALCOHOL)
# p-valor=7.409e-06<0,05: se rechaza la hipÃ³tesis de normalidad en la tasa de alcohol en chicos.
shapiro.test(TABLA_CHICAS$TASA_ALCOHOL)
# p-valor=0,1362>0,05: no se puede rechazar la hipÃ³tesis inical de que la tasa de alcohol en chicas siga una normal.

# GrÃ¡ficos de interacciÃ³n. ###########################################################################################
par(mfrow=c(1,2))
interaction.plot(TABLA_GLOBAL$SEXO, TABLA_GLOBAL$CANTIDAD_COPAS, TABLA_GLOBAL$NOTA_ATRACTIVO_LIGUE, fixed=TRUE)
interaction.plot(TABLA_GLOBAL$CANTIDAD_COPAS, TABLA_GLOBAL$SEXO, TABLA_GLOBAL$NOTA_ATRACTIVO_LIGUE, fixed=TRUE)

# AnÃ¡lisis de la varianza 
summary(aov(TABLA_GLOBAL$NOTA_ATRACTIVO_LIGUE~TABLA_GLOBAL$SEXO*TABLA_GLOBAL$CANTIDAD_COPAS, data=TABLA_GLOBAL))
# La cantidad de copas y su interacciÃ³n con el sexo afectan significativamente a la elecciÃ³n del atractivo del ligue.

# Comparaciones mÃºlltiples de tasas medias de alcohol.
TukeyHSD(aov(TABLA_GLOBAL$TASA_ALCOHOL~TABLA_GLOBAL$NACIONALIDAD, TABLA_GLOBAL))
plot(TukeyHSD(aov(TABLA_GLOBAL$TASA_ALCOHOL~TABLA_GLOBAL$NACIONALIDAD, TABLA_GLOBAL)), las=2, cex.axis=0.3)
# Existen diferencias significativas entre britÃ¡nic@s y suec@s, y entre britÃ¡nic@s e italian@s.

# REGRESIÃN LOGÃSTICA. ###############################################################################################
TABLA_GLOBAL$EXITO_NUM<-ifelse(TABLA_GLOBAL$EXITO=="SI", 1, 0)

# InicializaciÃ³n de la variable "colores", que reflejarÃ¡ si hubo o no hubo caso considerado Ã©xito.
colores<-NULL

# AsignaciÃ³n de color verde para casos con consideraciÃ³n de ÃXITO.
colores[TABLA_GLOBAL$EXITO_NUM==1]<-"green"

# AsignaciÃ³n de color rojo para casos con consideraciÃ³n de FRACASO.
colores[TABLA_GLOBAL$EXITO_NUM==0]<-"red"

# GrÃ¡fico de dispersiÃ³n relacionando la tasa de alcohol medida con terminar ligando o no.
plot(TABLA_GLOBAL$TASA_ALCOHOL, TABLA_GLOBAL$EXITO_NUM, pch=21, 
     bg=colores, xlab="Tasa de alcohol", ylab="Probabilidad de ligar")

# A la vista de los datos parece evidente conjeturar que el nivel de alcohol influye en la probabilidad de que 
# haya Ã©xito ligando. Ajustamos un modelo de regresiÃ³n logÃ­stica para estudiar la posible relaciÃ³n. 

# Empleamos el comando glm (para modelos lineales generalizados), indicando que la respuesta es binomial 
# (existe Exito ligando o no) mediante el argumento family:
modelo<-glm(EXITO_NUM~TASA_ALCOHOL, data=TABLA_GLOBAL, family=binomial)
summary(modelo)

# En el modelo de regresiÃ³n logÃ­stica la raÃ­z de las desviaciones representa el papel de los residuos.
# En la salida anterior estas cantidades se denominan deviance residuals. Calculamos estos pseudo-residuos:
(residuos_modelo=resid(modelo))

# CÃ¡lculo de las estimaciones para la probabilidad de Ã©xito (usando el comando predict); utilizamos un vector 
# conteniendo nuevas tasas de alcohol, que establecemos entre el mÃ­nimo observado (0,00) y el mÃ¡ximo observado 
# (1,89 grados), con saltos de longitud 0,01 para disponer asÃ­ de 190 nuesvos casos.
range(TABLA_GLOBAL$TASA_ALCOHOL)
Nuevos_Datos<-data.frame(TASA_ALCOHOL=seq(from=0.00, to=1.89, by=0.01))
Predicciones<-predict(modelo, newdata=Nuevos_Datos, type = "response")

# RepresentaciÃ³n grÃ¡fica de la funciÃ³n logÃ­stica estimada.
plot(TABLA_GLOBAL$TASA_ALCOHOL, TABLA_GLOBAL$EXITO_NUM, pch=21, 
     bg=colores, xlab="Tasa de alcohol", ylab="Probabilidad de Exito")

Nuevos_Datos<-seq(from=0.00, to=1.89, by=0.01)
lines(Nuevos_Datos, Predicciones, col="blue", lwd=2)

# Test de Wald para decidir si se puede afirmar que la tasa de alcohol influye en la probabilidad de Ã©xito.
wald.test(b=coef(modelo), Sigma=vcov(modelo), Terms=1)
# El p-valor 0,042 es inferior a 0,05, con lo que podemos asegurar que el efecto del alcohol es significativo: 
# mayores tasas implican menores probabilidades de Ã©xito al ligar.
######################################################################################################################

