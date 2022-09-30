valores<-c(2.1, 2.2, 1.8, 2, 1.9, 2.2, 2.6, 2.7, 2.5, 2.8, 1.8, 1.9, 1.6, 2, 1.9, 2.1, 2, 2.2, 2.4, 2.1)
valores

factor1<-gl(4, 5)
factor1

factor2<-factor(rep(1:5, 4))
factor2

xtabs(valores~factor1+factor2)


tapply(valores, factor1, summary)
tapply(valores, factor2, summary)

stripchart(valores~factor1, method="stack")
stripchart(valores~factor2, method="stack")

interaction.plot(factor1, factor2, valores, legend=F)
interaction.plot(factor2, factor1, valores, legend=F)

# Observamos líneas paralelas, por lo que no parece existir interacción entre factores.

analisis<-lm(valores~factor1+factor2)
anova(analisis)

# El factor 1 resulta significativo.




