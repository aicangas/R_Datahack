# Construcción de la matriz con los datos.
Convictions<-matrix(c(2, 10, 15, 3),
                     nrow=2,
                     dimnames=list(c("Dizygotic", "Monozygotic"),
                                   c("Convicted", "Not convicted")))
Convictions

# Se pretende decidir si la distribución de frecuencias difiere entre grupos 
# o si se da alguna asociación entre las variables "Estar o no condenado" -vs- "Tipo de gemelo".

# Si el ser de uno u otro tipo de gemelo no afectase a estar o no condenado, esperaríamos que las 
# proporciones de cada caso fuesen las mismas. Por eso establecemos la siguiente hipótesis nula:

# Hipótesis nula: las proporciones de condenados monocigóticos y condenados dicigóticos son la misma.

# Prueba de independencia Chi-Cuadrado.
chisq.test(Convictions)
# p-valor=0,001221<0,05: rechazamos la hipótesis nula (de independencia entre variables). 
# Por tanto, existe una relación relevante entre el tipo de gemelos y el estar o no condenado.

# Calculamos ahora las frecuencias esperadas en cada caso:
chisq.test(Convictions)$expected
# Estas frecuencias surgen de hacer la multiplicación de las cantidades marginales de cada caso y
# dividir después por la cantidad total de casos de estudio:
# (17 casos de dicigóticos)*(12 casos de condenados)/(30 casos totales)=6,8.
# (17 casos de dicigóticos)*(18 casos de no condenados)/(30 casos totales)=10,2.
# (13 casos de monocigóticos)*(12 casos de condenados)/(30 casos totales)=5,2.
# (13 casos de monocigóticos)*(18 casos de no condenados)/(30 casos totales)=7,8.

# Frecuencias reales:
# > Convictions
#                 Convicted Not convicted
# Dizygotic           2            15
# Monozygotic        10             3

# Frecuencias esperadas:
# > chisq.test(Convictions)$expected
#                 Convicted Not convicted
# Dizygotic         6.8          10.2
# Monozygotic       5.2           7.8

# A tenor de las dos tablas anteriores, la proporción de condenados monocigóticos es mayor de lo esperado.
# Por otro lado, tenemos una frecuencia esperada que es ligeramente superior a 5 (monocigóticos condenados),
# con lo que casi se cumple el hecho de tener más del 20% de las frecuencias esperadas menores que 5.

# En tal caso, y al tratarse de una tabla 2x2, para más precisión acudimos a la prueba exacta de Fisher:

# Prueba exacta de Fisher.
# Hipótesis nula: las variables son independientes, no existe asociación entre 
# "Tipo de gemelos" (monocigógicos o dicigóticos) y "Estado de condena" (condenado o no condenado).
fisher.test(Convictions)
# p-value=0,0005367<0,05: rechazamos la hipótesis nula.
# Existe relación entre el tipo de gemelos y el estar o no condenado, 
# la proporción de condenados es relevantemente mayor para los gemelos monocigóticos que para los dicigóticos.

