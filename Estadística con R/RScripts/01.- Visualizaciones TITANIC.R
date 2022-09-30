# -----------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------

function_install_packages <- function(x)
{
  for(i in x)
  {
    if(!require(i, character.only = TRUE))
    {
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
}

function_install_packages(c("dplyr", "ggplot2", "corrplot", "ggcorrplot", "e1071", "GGally", "tidyverse", "titanic", "ggpubr", "vcd"))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

DATOS <- titanic_train
DATOS <- as.data.frame(titanic_train)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

table(DATOS$Survived)
DATOS$SUPERVIVENCIA <- ifelse(DATOS$Survived == 1, "SUPERVIVIENTE", "NO SUPERVIVIENTE")
table(DATOS$SUPERVIVENCIA)
DATOS$SUPERVIVENCIA <- as.factor(DATOS$SUPERVIVENCIA)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

table(DATOS$Pclass)
DATOS$CLASE <- ifelse(DATOS$Pclass == 1, "PRIMERA CLASE",
               ifelse(DATOS$Pclass == 2, "SEGUNDA CLASE", 
               ifelse(DATOS$Pclass == 3, "TERCERA CLASE", "NULL")))

table(DATOS$CLASE)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

(DATOS_FRECUENCIAS_SUPERVIVENCIA_CLASE_Y_SEXO <- DATOS %>% group_by(CLASE, Sex, SUPERVIVENCIA) %>% summarise(FRECUENCIAS = n()))

DATOS_FRECUENCIAS_SUPERVIVENCIA_CLASE_Y_SEXO <- as.data.frame(DATOS_FRECUENCIAS_SUPERVIVENCIA_CLASE_Y_SEXO)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

GRAFICO_HISTOGRAMA_SUPERVIVENCIA <- ggplot(data = DATOS, aes(x = SUPERVIVENCIA, fill = SUPERVIVENCIA)) + 
                                    geom_bar(stat = "count") + 
                                    scale_fill_manual(values = c("azure3", "blueviolet")) + 
                                    labs(title = "Supervivencia") +
                                    theme(legend.position = "right") + 
                                    theme_bw() 

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

GRAFICO_BALLOON_SUPERVIVENCIA_CLASE_SEXO <- ggballoonplot(DATOS_FRECUENCIAS_SUPERVIVENCIA_CLASE_Y_SEXO, x = "CLASE", y = "Sex", size = "FRECUENCIAS",
                                            fill = "FRECUENCIAS", facet.by = "SUPERVIVENCIA",
                                            ggtheme = theme_bw()) +
                                            scale_fill_viridis_c(option = "C")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

GRAFICO_DENSIDAD_EDAD <- ggplot(data = DATOS, aes(x = Age, fill = SUPERVIVENCIA)) + 
                         geom_density(alpha = 0.5) + 
                         scale_fill_manual(values = c("azure3", "blueviolet")) + 
                         geom_rug(aes(color = SUPERVIVENCIA), alpha = 0.5) + 
                         scale_color_manual(values = c("azure3", "blueviolet")) + 
                         theme_bw() 

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

GRAFICO_VIOLIN_EDAD <- ggplot(data = DATOS, aes(x = SUPERVIVENCIA, y = Age, fill = SUPERVIVENCIA)) + 
                       geom_violin(color = "darkred") + 
                       geom_jitter(alpha = 0.5, width = 0.10) +
                       scale_fill_manual(values = c("azure3", "blueviolet")) +
                       geom_boxplot(width = 0.1) + 
                       theme_bw()

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

GRAFICOS_CONJUNTO <- ggarrange(GRAFICO_HISTOGRAMA_SUPERVIVENCIA, 
                               GRAFICO_BALLOON_SUPERVIVENCIA_CLASE_SEXO,
                               GRAFICO_DENSIDAD_EDAD,
                               GRAFICO_VIOLIN_EDAD, 
                               
                               legend = "top")
  
  
GRAFICOS_CONJUNTO <- annotate_figure(GRAFICOS_CONJUNTO, top = text_grob("RESUMEN GRÁFICO ESTUDIO TITANIC", size = 15))

GRAFICOS_CONJUNTO

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
