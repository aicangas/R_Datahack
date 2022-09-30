

# Cantidades de Impactos.
imp<-c(seq(0,5))
imp

# Cantidad de bombas.
obs<-c(229,211,93,35,7,1)
obs

# Total de bombas.
n<-sum(obs)
n

# Parámetro lambda para Distribución de Poisson.
lambda<-sum(imp*obs)/n
lambda

# Distribución de Poisson.
prob<-dpois(imp,lambda)
prob
sum(prob)

# Frecuencias esperadas.
esp<-n*prob
esp


# Test Chi Cuadrado.
chisq.test(obs,prob)


