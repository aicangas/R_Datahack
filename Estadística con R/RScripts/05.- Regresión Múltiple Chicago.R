# race: la composici?n racial en porcentaje minoritario.
# fire: incendios por cada 100 unidades de vivienda.
# theft: robo por cada 1000 habitantes.
# age: porcentaje de las viviendas construidas antes de 1939.
# volact: nuevas p?lizas de vivienda, adem?s de las renovaciones menos cancelaciones y no renovaciones por cada 100 unidades de vivienda.
# involact: nuevas p?lizas del plan FAIR y renovaciones por cada 100 unidades de vivienda.
# income: ingreso medio familiar.

# Queremos explicar la variable "involact" en funci?n de las dem?s variables, 
# excepto "volact", y con "income" en logaritmo. 

# Carga de librer?as.
library(faraway)
library(ppcor)
library(WRS2)
library(relaimpo)
library(hier.part)
library(car)
# library(robustbase)
# library(rapportools)
library(lmtest)
library(stats)
# library(rrcov)

# Carga de datos.
data(chicago)
head(chicago)

# Construimos un nuevo data set con income en logaritmo (neperiano).
chicago_log<-transform(chicago, logincome=log(income))
head(chicago_log)

# Descartamos el inicial income y tambi?n la variable volact; 
# consideraremos ?nicamente los siguientes atributos:
# VARIABLE DEPENDIENTE: involact.
# VARIABLES INDEPENDIENTES: race, fire, theft, age y logincome (logaritmo neperiano de income). 
chicago_ejercicio<-chicago_log[,c(-5,-7)]
head(chicago_ejercicio)
# De este modo, el data set protagonista en adelante ser? chicago_ejercicio.

attach(chicago_ejercicio)
# 1.- MATRIZ DE CORRELACIONES PARA TODAS LAS VARIABLES Y GR?FICOS DE DISPERSI?N. 

# Inspecci?n de los datos: diagramas de dispersi?n entre todos los pares de variables.
pairs(chicago_ejercicio, pch=20)
# Se intuye a trav?s de los gr?ficos que podr?a existir cierta relaci?n directa entre 
# involact y race, e involact y fire; e inversa entre involact y logincome; adicionalmente, 
# race y logincome, y fire y logincome, tambi?n podr?an mantener una relaci?n inversa.
# Por ?ltimo, gr?ficamente y con relaci?n a lo anterior, se desprende una supuesta relaci?n
# directa entre race y fire.

# 1.1.- Correlaci?n m?ltiple cl?sica.

# Matriz de correlaciones entre todos los pares de variables.
cor(chicago_ejercicio)
# Las siguientes correlaciones son las 6 m?s trascendentes, tanto positivas como negativas,
# y vienen al hilo de lo visto en los gr?ficos y descrito anteriormente.
# fire-race:           0,5927956 
# involact-race :      0,7137540
# involact-fire :      0,7030397
# involact-logincome: -0,7041241
# race-logincome:     -0,7653099
# fire-logincome:     -0,6836457


# 2.- MODELO DE REGRESI?N LINEAL M?LTIPLE CL?SICO.

# Comenzamos por establecer el modelo de regresi?n lineal m?ltiple cl?sico con todas las 
# variables. Atenderemos a los p-valores asociados a las variables independientes y 
# descartaremos secuencialmente aqu?llas que no sean representativas para terminar generando 
# un modelo final. Evaluaremos el ajuste y la bondad de cada uno de los modelos para decidir 
# cu?l es el m?s apropiado.

modelo<-lm(involact~race+fire+theft+age+logincome, data=chicago_ejercicio)
summary(modelo)
# Ateni?ndonos a los coeficientes, la ecuaci?n del modelo ser?a la siguiente: 
# modelo: 
# involact=-3,573976+0,009502*race+0,039856*fire-0,010295*theft+0,008336*age+0,345762*logincome.


# 3.- EVALUACI?N DEL AJUSTE GLOBAL DEL MODELO Y DE LA BONDAD DEL AJUSTE.

# Se tiene que F(5 , 41)=24,83 y p-valor=2,009e-11 (pr?cticamente nulo), 
# con lo que el modelo es significativo.
# La bondad del ajuste es medianamente alta (R^2 ajustado es 0,7214).

# Reajustamos el modelo eliminando toda variable que no sea significativa; en este caso nos 
# centramos en "logincome", pues su p-valor es el m?s elevado: 0,392540>0,05.
modelo2<-update(modelo, .~.-logincome)
summary(modelo2)
# Descartar la variable logincome no representa consecuencias notorias: 
# el modelo sigue siendo significativo y la bondad del ajuste queda pr?cticamente igual 
# (sube ligeramente, hasta 0,7231).

# Sab?amos adem?s que logincome ten?a correlaci?n significativa con otras dos variables
# predictoras (race y fire).

# Comparaci?n de los dos modelos.
anova(modelo, modelo2)
# p-valor=0,3925>0,05: eliminar la variable logincome no supone efectos relevantes en el modelo.
# El modelo resultante es entonces el siguiente: 
# modelo2: involact=-0,243118+0,008104*race+0,036646*fire-0,009592*theft+0,007210*age.

# A su vez podemos reajustar este modelo2 anterior, pues el intercepto tampoco resulta 
# significativo (p-valor=0,101158>0,05).
modelo3<-update(modelo2, .~.-1)
summary(modelo3)
# Ahora todas las variables s? resultan ser significativas en este modelo3.
# Descartar el intercepto no hace que el modelo deje de ser significativo (p-valor sigue siendo 
# muy cercano a cero), y la bondad del ajuste ahora aumenta notoriamente: R^2 ajustado=0,8497.

# Comparaci?n de los dos modelos anteriores.
anova(modelo2, modelo3)
# p-valor=0,1012>0,05: eliminar el intercepto no supone efectos relevantes en el modelo.

# El modelo final es por tanto el que sigue:
# modelo3: involact=0,007662*race+0,037965*fire-0,010567*theft+0,004144*age.

# Mostramos adem?s los intervalos de confianza a distintos niveles de significaci?n 
# para los coeficientes estimados:
confint(modelo3)

# Criterios para seleccionar el mejor modelo.

# Para decidir qu? n?mero de variables predictoras es el adecuado utilizamos el AIC o 
# Criterio de Informaci?n de Akaike: mediante la funci?n step seleccionaremos aqu?l que 
# muestre el AIC con valor m?nimo. 

# Funci?n step en su formato hacia adelante.
reg.lm0<-lm(involact~1, chicago_ejercicio)
slm.forward<-step(reg.lm0, scope=~race+fire+theft+age, direction="forward")
# El modelo m?s apropiado es el que tiene AIC con valor m?nimo; en este caso, se 
# trata del modelo3, que efectivamente contempla involact~race+fire+theft+age.


# 4.- ESTIMACI?N DE LA IMPORTANCIA RELATIVA DE LOS PREDICTORES.

# Medida de la bondad del ajuste con RMSPE (Root-Mean-Square Prediction Error).
relat.imp.RMSPE=hier.part(chicago_ejercicio$involact, chicago_ejercicio[, 1:4], 
                          family="gaussian", gof="RMSPE", barplot=TRUE)
relat.imp.RMSPE
# Importancias relativas seg?n RMSPE.
# race  38.66%
# fire  38.88%
# theft  7.61%
# age   14.85%

# Los dos predictores con mayor importancia relativa son race y fire, yendo acorde a lo
# indicado inicialmente sobre las grandes correlaciones existentes entre involact y 
# esas dos variables dependientes y significativas en el modelo.


# 6.- DIAGN?STICO DEL MODELO Y PRESENCIA DE OUTLIERS O DATOS AT?PICOS.

# Comenzamos viendo que se satisface uno de los supuestos del modelo lineal, pues
# existe independencia de los residuos.

# Prueba de Durbin-Watson. H0: no existe autocorrelaci?n.
dwtest(modelo3)
# p-valor=0,5582>0,05: Aceptamos H0. 
# No existe autocorrelaci?n (DW=2,06822~2: residuos casi completamente independientes). 
# Se satisface el supuesto del modelo lineal.

par(mfrow=c(2,2))
plot(modelo3)

# 6.1.- Gr?fico Residuals -vs- Fitted: residuos frente a valores ajustados o predichos. 

# La variabilidad de los residuos parece aumentar con los valores ajustados, aparentemente 
# no existe azar en la dispersi?n del gr?fico.

# Vemos, adem?s, que aparecen tres outliers: c?digos postales 60610, 60613 y 60621.

# Con el resultado anterior se antoja recomendable acudir al modelo robusto.

# 6.2.- Gr?fico Normal Q-Q: Gr?fico cuantil-cuantil normal de los residuos estandarizados. 

# El modelo de regresi?n lineal asume que los errores tienen comportamiento normal, 
# y en consecuencia su representaci?n sigue la l?nea recta que se distingue en el gr?fico.

# Con el test de normalidad de Shapiro-Wilk vemos que los residuos siguen una distribuci?n normal.
# H0: los residuos muestran comportamiento normal.
shapiro.test(resid(modelo3))
# p-valor=0,2427>0,05, por lo que no rechazamos H0, los residuos se comportan como una normal.
# Se cumple el supuesto del modelo lineal.


# 6.3.- Gr?fico Scale-Location.

# Argumento an?logo al desarrollado en el primero de los gr?ficos, considerando en este caso
# la ra?z cuadrada de los residuos estandarizados.

# 6.4.- Gr?fico Residuals -vs- Leverage: distancias de Cook.

# Graficamos los outliers sobre las variables analizadas.
cooks=cooks.distance(modelo3)
hat<-lm.influence(modelo3)$hat

par(mfrow=c(2,1))
plot(modelo3, col=ifelse(cooks>quantile(cooks,.90),'red','black'),pch=20)
plot(modelo3, col=ifelse(hat>quantile(hat,.90),'green','black'),pch=20)

par(mfrow=c(2,1))
plot(race+fire+theft+age,predict(modelo3),
     col=ifelse(cooks>quantile(cooks,.9),'red','black'),pch=20)
plot(race+fire+theft+age,predict(modelo3),
     col=ifelse(hat>quantile(hat,.9),'green','black'),pch=20)

# Existen outliers pero ninguno se sit?a m?s all? de la l?nea que marca la distancia de Cook
# igual a 1, con lo que siendo influyentes no se puede considerar que su influencia sea alta.


# RESUMEN DE ECUACIONES DE LOS MODELOS DE REGRESI?N M?LTIPLES:

# Modelo cl?sico:  involact=0,007662*race+0,037965*fire-0,010567*theft+0,004144*age
# Modelo robusto:  involact=0,004858*race+0,047931*fire-0,007258*theft


