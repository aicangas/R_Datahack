# Carga de librer?as.
library(WRS2)

# Carga de datos.
data(goggles)

# Visualizaci?n de la distribuci?n de los datos.
par(cex=.6)
boxplot(attractiveness~gender*alcohol, data=goggles, las=2)

# ANOVA cl?sico.
summary(aov(attractiveness~gender*alcohol, data=goggles))

# M?todos robustos.

# Comparaci?n de las medias recortadas.
t2way(attractiveness~gender*alcohol, data=goggles)

# Comparaci?n de las medianas.
med2way(attractiveness~gender*alcohol, data=goggles)

# Comparaci?n del M-estimador One-Step.
pbad2way(attractiveness~gender*alcohol, data=goggles, est="onestep")

# Gr?ficos de interacci?n.
par(mfrow=c(1,2),cex=.6)
interaction.plot(gender, alcohol, attractiveness, fixed=TRUE, las=2)
interaction.plot(alcohol, gender, attractiveness, fixed=TRUE, las=2)

# Comparaciones m?ltiples post-hoc.
mcp2atm(attractiveness~gender*alcohol, data=goggles)
mcp2a(attractiveness~gender*alcohol, data=goggles)
mcp2a(attractiveness~gender*alcohol, data=goggles, est="median")

# Las conquistas de los hombres son menos atractivas tras 4 pintas. 
# El resultado m?s interesante es la comparaci?n posthoc gender1:alcohol3, 
# pues explica la ca?da en la atracci?n para 4 pintas en hombres.



