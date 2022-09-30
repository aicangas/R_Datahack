# Librería para generar los gráficos.
library(ggplot2)

# Tabla de datos que será la base de los gráficos.
data(CO2)
names(CO2)

# La variable Type será el equivalente a la provincia de la práctica.
table(CO2$Type)

# Preparación previa para definir el ancho de las barras siguiendo el criterio de Freedman-Diaconis.
breaks <- pretty(range(CO2[ , "uptake"]), n = nclass.FD(CO2[ , "uptake"]), min.n = 1)
ancho_barra <- breaks[2] - breaks[1]

# Primer gráfico: barras separadas (dodge) con el criterio de Sturges.
ggplot(data = CO2, aes(x = uptake, group = Type, fill = Type)) +
geom_histogram(position = "dodge", bins = 1 + (3.322 * log10(nrow(CO2)/2)), color = "black", alpha = 0.5) +
labs(x = "Valor de Uptake", y = "Frecuencia", fill = "Ciudad") +
theme_bw()

# Segundo gráfico: barras solapadas (identity) con el criterio de Sturges.
ggplot(data = CO2, aes(x = uptake, group = Type, fill = Type)) +
geom_histogram(position = "identity", bins = 1 + (3.322 * log10(nrow(CO2)/2)), color = "black", alpha = 0.5) +
labs(x = "Valor de Uptake", y = "Frecuencia", fill = "Ciudad") +
theme_bw()

# Tercer gráfico: barras separadas (dodge) con el criterio de Freedman-Diaconis.
ggplot(data = CO2, aes(x = uptake, group = Type, fill = Type)) +
geom_histogram(position = "dodge", binwidth = ancho_barra, color = "black", alpha = 0.5) +
labs(x = "Valor de Uptake", y = "Frecuencia", fill = "Ciudad") +
theme_bw()

# Cuarto gráfico: barras solapadas (identity) con el criterio de Freedman-Diaconis.
ggplot(data = CO2, aes(x = uptake, group = Type, fill = Type)) +
geom_histogram(position = "identity", binwidth = ancho_barra, color = "black", alpha = 0.5) +
labs(x = "Valor de Uptake", y = "Frecuencia", fill = "Ciudad") +
theme_bw()


