# Carga de librerías necesarias.
# library(ggplot2)
# library(readr)
# library(gridExtra)
# library(grid)
# library(dplyr)
library(e1071)
library(readxl)

# Lectura de datos.
Dataset_01 <- read_excel("C:/Users/ivang/Desktop/Machine Learning & Math Backup/Datahack/BCDS MAY2020/Tablas/Dataset 01.xlsx")

# Primera aproximación a los datos.
str(Dataset_01)
names(Dataset_01)
summary(Dataset_01)
head(Dataset_01)


# Mínimo, primer cuartil, mediana, tercer cuartil y máximo.
fivenum(Dataset_01$V2)
fivenum(Dataset_01$V1)

# Estadísticas de las variables.
boxplot.stats(Dataset_01$V2)
boxplot.stats(Dataset_01$V1)

mean(Dataset_01$V2)
median(Dataset_01$V2)
mode(Dataset_01$V2)
var(Dataset_01$V2)
sd(Dataset_01$V2)
skewness(Dataset_01$V2)
kurtosis(Dataset_01$V2)

# Histogramas.
Histograma<-ggplot(data=Dataset_01, aes(x=V2))+
                   geom_histogram(binwidth=20, color="black", aes(fill=Pais))+ 
                   xlab("Rango de la variable V2")+  
                   ylab("Cantidad de registros")+
                   theme(legend.position="top")+
                   #theme(legend.position="none")+
                   ggtitle("Histograma de la variable V2")
Histograma                   

# Gráfico de dispersión.
plot(x=Dataset_01$V1, y=Dataset_01$V2, 
     xlab="Variable V1", ylab="Variable V2", main="Variables V1 y V2")

# Gráfico de dispersión mejorado.
grafico<-ggplot(data=Dataset_01, aes(x=V1, y=V2))
grafico+geom_point(aes(color=Pais))+xlab("Variable V1")+ylab("Variable V2")+ggtitle("Variables V1 y V2")

# Diagrama de cajas.
boxplot(V1~Pais,data=Dataset_01, 
        xlab="Pais", ylab="Variable V1", main="Dataset_01 Boxplot")

# -----------------------------------------------------------------------------------------------------------------

TABLA_ESPANA <- Dataset_01 %>% filter(Pais == "España")
boxplot.stats(TABLA_ESPANA$V1)
min(TABLA_ESPANA$V1)

boxplot.stats(TABLA_ESPANA$V1)$stats[1]

TABLA_ANOMALOS_POR_DEBAJO_ESPANA <- TABLA_ESPANA %>% filter(V1 < boxplot.stats(TABLA_ESPANA$V1)$stats[1])

# -----------------------------------------------------------------------------------------------------------------

boxplot.stats(TABLA_ESPANA$V2)
max(TABLA_ESPANA$V2)
boxplot.stats(TABLA_ESPANA$V2)$stats[5]

TABLA_ANOMALOS_POR_ARRIBA_ESPANA <- TABLA_ESPANA %>% filter(V2 > boxplot.stats(TABLA_ESPANA$V2)$stats[5])
TABLA_ANOMALOS_POR_ARRIBA_ESPANA <- TABLA_ESPANA %>% filter(V2 > boxplot.stats(TABLA_ESPANA$V2)$stats[5])


# -----------------------------------------------------------------------------------------------------------------

summary(Dataset_01$V1)
  
# Diagrama de cajas mejorado.
# Definimos una función para construir con más detalle el diagrama de cajas de cada variable cuantitativa.
boxplot2<-function(x,y)
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
  text(rep(0.75,2),stats3,labels=c('MÍNIMO','MÁXIMO'))
}

# Visualizamos los diagramas de caja para las cuatro variables cuantitativas.
par(mfrow=c(1,2))
boxplot2(Dataset_01$V1, 'Diagrama de cajas para la variable V1')
boxplot2(Dataset_01$V2, 'Diagrama de cajas para la variable V2')


skewness(Dataset_01$V2)

# Diagramas de caja según Pais.
box<-ggplot(data=Dataset_01, aes(x=Pais, y=V1))
box+geom_boxplot(aes(fill=Pais))+
  ylab("Variable V1")+ggtitle("Diagramas de Cajas según Pais")+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)



