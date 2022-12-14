# Librer?a para generar los gr?ficos.
library(ggplot2)

# Tabla de datos que ser? la base de los gr?ficos.
data(CO2)
names(CO2)

# La variable Type ser? el equivalente a la provincia de la pr?ctica.
table(CO2$Type)

# Preparaci?n previa para definir el ancho de las barras siguiendo el criterio de Freedman-Diaconis.
breaks <- pretty(range(CO2[ , "uptake"]), n = nclass.FD(CO2[ , "uptake"]), min.n = 1)
ancho_barra <- breaks[2] - breaks[1]

# Primer gr?fico: barras separadas (dodge) con el criterio de Sturges.
ggplot(data = CO2, aes(x = uptake, group = Type, fill = Type)) +
geom_histogram(position = "dodge", bins = 1 + (3.322 * log10(nrow(CO2)/2)), color = "black", alpha = 0.5) +
labs(x = "Valor de Uptake", y = "Frecuencia", fill = "Ciudad") +
theme_bw()

# Segundo gr?fico: barras solapadas (identity) con el criterio de Sturges.
ggplot(data = CO2, aes(x = uptake, group = Type, fill = Type)) +
geom_histogram(position = "identity", bins = 1 + (3.322 * log10(nrow(CO2)/2)), color = "black", alpha = 0.5) +
labs(x = "Valor de Uptake", y = "Frecuencia", fill = "Ciudad") +
theme_bw()

# Tercer gr?fico: barras separadas (dodge) con el criterio de Freedman-Diaconis.
ggplot(data = CO2, aes(x = uptake, group = Type, fill = Type)) +
geom_histogram(position = "dodge", binwidth = ancho_barra, color = "black", alpha = 0.5) +
labs(x = "Valor de Uptake", y = "Frecuencia", fill = "Ciudad") +
theme_bw()

# Cuarto gr?fico: barras solapadas (identity) con el criterio de Freedman-Diaconis.
ggplot(data = CO2, aes(x = uptake, group = Type, fill = Type)) +
geom_histogram(position = "identity", binwidth = ancho_barra, color = "black", alpha = 0.5) +
labs(x = "Valor de Uptake", y = "Frecuencia", fill = "Ciudad") +
theme_bw()


