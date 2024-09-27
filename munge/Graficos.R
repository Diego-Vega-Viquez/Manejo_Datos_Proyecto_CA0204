
# Acá van los códigos de algunos graficos que generemos

# Librerías:

install.packages("ProjectTemplate")
install.packages("devtools")
install.packages("tidyverse")
install.packages("cowplot")
install.packages("openintro")

library("digest")
library("tibble")
library(ProjectTemplate)
library(tidyverse)
library(cowplot)
# Estos son necesarios para cargar openintro
library(airports)
library(cherryblossom)
library(usdata)
library(openintro)
library(haven) # Para cargar el .sav
library(scales)


datos_con_tipo_de_varibles %>%
  filter(!is.na(A16B)) %>%  # Excluye valores NA
  ggplot(aes(x = A16B)) +
  geom_bar(colour = "black") +
  labs( x = "Títulos", 
        y = "Cantidad")

variables_utiles %>% ggplot(aes(x = NivInst,
                                y = ipnh,
                                fill = NivInst)) +
                     geom_boxplot() +
                     scale_y_log10(labels = label_number()) +
                     labs( title = "Figura 1. Comparación del Ingreso Principal Neto del Hogar según Nivel de Instrucción, 2023",
                           subtitle = "Expresado en colones costarricenses",
                           x = "Nivel de Instrucción",
                           y = "Ingreso Principal Neto del Hogar",
                           fill =  "Nivel de Instrucción",
                           caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
                     theme(axis.text.x = element_blank())

