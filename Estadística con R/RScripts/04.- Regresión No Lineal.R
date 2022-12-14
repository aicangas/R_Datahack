#Antes de cargar los datos, comprobar que no tengamos valores de la variable dependiente
#o independiente igual a 0 (cambiar por ejemplo a 0.01 o eliminar fila)

datos<-read.table("datos.txt",header=T,blank.lines.skip=F)

#Ajuste cuadr?tico o polin?mica de orden 2
reg<-lm(H_1~X+I(X^2),data=datos); summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste cuadr?tico o polin?mica de orden 2')
curve(reg$coefficient[1]+reg$coefficient[2]*x+reg$coefficient[3]*x^2,add=T,col="red")

#Ajuste c?bico o polin?mica de orden 3
reg<-lm(H_1~X+I(X^2)+I(X^3),data=datos); summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste c?bico o polin?mica de orden 3')
curve(reg$coefficient[1]+reg$coefficient[2]*x+reg$coefficient[3]*x^2+reg$coefficient[4]*x^3,add=T)

#Ajuste logar?tmico:
tras<-log(datos$X)
reg<-lm(H_1~tras,data=datos); summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste logar?tmico')
curve(reg$coefficient[1]+reg$coefficient[2]*log(x),add=T,col="violet")

#Ajuste potencia:
trasX<-log(datos$X);trasY<-log(datos$H_1)
reg<-lm(trasY~trasX,data=datos);summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste potencia')
curve(exp(reg$coefficient[1])*x^(reg$coefficient[2]),add=T,col='purple')

#Ajuste Curva-S.
tras<-log(datos$H_1)
reg<-lm(tras~I(1/X),data=datos);summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste Curva-S')
curve(exp(reg$coefficient[1]+(reg$coefficient[2])/x),add=T,col="blue")

#Ajuste inversa.
reg<-lm(H_1~I(1/X),data=datos); summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste inversa')
curve(reg$coefficient[1]+reg$coefficient[2]/x,add=T,col="green")

#Ajuste exponencial:
tras<-log(datos$H_1)
reg<-lm(tras~X,data=datos);summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste exponencial')
curve(exp(reg$coefficient[1])*exp(reg$coefficient[2]*x),add=T,col="orange")

#Ajuste compuesto:
tras<-log(datos$H_1)
reg<-lm(tras~X,data=datos);summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste compuesto')
curve(exp(reg$coefficient[1])*exp(reg$coefficient[2])^x,add=T,col="gray")

#Ajuste crecimiento:
tras<-log(datos$H_1)
reg<-lm(tras~X,data=datos);summary(reg)
plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste crecimiento')
curve(exp(reg$coefficient[1]+reg$coefficient[2]*x),add=T)