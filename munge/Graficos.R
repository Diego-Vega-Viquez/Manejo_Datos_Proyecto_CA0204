
# Acá van los códigos de algunos graficos que generemos

# Librerías:

install.packages("ProjectTemplate")
install.packages("devtools")
install.packages("tidyverse")
install.packages("cowplot")
install.packages("openintro")
install.packages("cowplot")
install.packages("ggridges")

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
library(ggridges) # Para usar geom_density_ridges


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

variables_utiles %>% filter(spmn > 0) %>% ggplot(aes(x = spmn, 
                                                     y = A16B, 
                                                     fill = A16B, 
                                                     color = A16B)) + 
                     geom_density_ridges(alpha = 0.5) + 
                     scale_x_log10(labels = label_number()) +
                      labs( title = "Figura 2. Distribución del Ingreso Total por persona del Hogar según Grado Académico en Costa Rica, 2023",
                            subtitle = "Expresado en colones costarricenses",
                            x = "Ingreso total del hogar neto",
                            y = "Título",
                            fill = "Título",
                            color = "Título",
                            caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
                      guides(
                        fill = guide_legend(reverse = TRUE),  
                        color = guide_legend(reverse = TRUE)) + 
                     theme(axis.text.y = element_blank())

variables_utiles %>% filter(!is.na(Q_IPCN)) %>% 
                     ggplot(aes(x = Escolari, 
                                y = ipsnt, 
                                colour = Q_IPCN)) +
                      geom_jitter() +
                      geom_smooth(method = "lm", se = FALSE, colour = "black") +
                      facet_wrap(~Q_IPCN) +
                      labs(title = "Relación entre los años de escolaridad e ingreso salario neto",
                           x = "Años de escolaridad",
                           y = "Ingreso principal salario neto total",
                           colour = "Quintiles de ingreso per cápita \ndel hogar neto \npor región de planificación",
                           caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
                     scale_y_log10(labels = label_number()) +
                     theme_cowplot() 
variables_utiles %>% filter(!is.na(A22A)) %>%
                     ggplot(aes(x = A22A, 
                                y = ipcn)) + 
                     geom_boxplot() +
                     scale_y_log10(labels = label_number())

## USAR ForReg que es el rezago