# Carga de librerías.
library(WRS2)

# Carga de datos.
data(goggles)

# Visualización de la distribución de los datos.
par(cex=.6)
boxplot(attractiveness~gender*alcohol, data=goggles, las=2)

# ANOVA clásico.
summary(aov(attractiveness~gender*alcohol, data=goggles))

# Métodos robustos.

# Comparación de las medias recortadas.
t2way(attractiveness~gender*alcohol, data=goggles)

# Comparación de las medianas.
med2way(attractiveness~gender*alcohol, data=goggles)

# Comparación del M-estimador One-Step.
pbad2way(attractiveness~gender*alcohol, data=goggles, est="onestep")

# Gráficos de interacción.
par(mfrow=c(1,2),cex=.6)
interaction.plot(gender, alcohol, attractiveness, fixed=TRUE, las=2)
interaction.plot(alcohol, gender, attractiveness, fixed=TRUE, las=2)

# Comparaciones múltiples post-hoc.
mcp2atm(attractiveness~gender*alcohol, data=goggles)
mcp2a(attractiveness~gender*alcohol, data=goggles)
mcp2a(attractiveness~gender*alcohol, data=goggles, est="median")

# Las conquistas de los hombres son menos atractivas tras 4 pintas. 
# El resultado más interesante es la comparación posthoc gender1:alcohol3, 
# pues explica la caída en la atracción para 4 pintas en hombres.



