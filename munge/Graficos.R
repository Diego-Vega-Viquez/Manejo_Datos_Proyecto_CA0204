
# Acá van los códigos de algunos graficos que generemos

# Librerías:

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
library(sf) 
library(RColorBrewer)
library(see)
library(GGally)
library(kableExtra)

##############
# GRAFICO 12 #
##############

variables_utiles %>% ggplot(aes(x = NivInst,
                                y = ipnh,
                                fill = NivInst)) +
                     geom_boxplot() +
                     scale_y_log10(labels = label_number()) +
                     labs( title = "Gráfico 12. \nComparación del Ingreso Principal Neto del Hogar según Nivel de Instrucción, en Costa Rica al 2023",
                           subtitle = "Expresado en colones costarricenses",
                           x = "Nivel de Instrucción",
                           y = "Ingreso Principal Neto del Hogar",
                           fill =  "Nivel de Instrucción",
                           caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
                     coord_flip() + 
                     theme(legend.position = "none")

ggsave("../Manejo_de_Datos/graphs/Grafico12.2.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.5, # Tamaño: 11.5 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

##############
# GRAFICO 13 #
##############

variables_utiles %>% filter(spmn > 0 &!is.na(A16B)) %>% ggplot(aes(x = spmn, 
                                                     y = A16B, 
                                                     fill = A16B, 
                                                     color = A16B)) + 
                     geom_density_ridges(alpha = 0.5) + 
                     scale_x_log10(labels = label_number()) +
                      labs( title = "Gráfico 13. \nDistribución del Ingreso Total por persona del Hogar según Grado Académico en Costa Rica, 2023",
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

ggsave("../Manejo_de_Datos/graphs/Grafico13.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 13, # Tamaño: 11.5 pulgadas de ancho
       height = 8, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

##############
# GRAFICO 14 #
##############

variables_utiles %>% filter(!is.na(Q_IPCN)) %>% 
                     ggplot(aes(x = Escolari, 
                                y = ipsnt, 
                                colour = Q_IPCN)) +
                      geom_jitter() +
                      geom_smooth(method = "lm", se = FALSE, colour = "black") +
                      facet_wrap(~Q_IPCN) +
  labs(title = "Gráfico 14. \nRelación entre los años de escolaridad e ingreso salario neto",
       x = "Años de escolaridad",
       y = "Ingreso principal salario neto total",
       colour = "Quintiles de ingreso per cápita \ndel hogar neto \npor región de planificación",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_y_log10(labels = label_number()) +
  scale_colour_brewer(palette = "Set1") +  # Paleta de colores para los quintiles
  theme_cowplot()

ggsave("../Manejo_de_Datos/graphs/Grafico14.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 16, # Tamaño: 11.5 pulgadas de ancho
       height = 8, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

## USAR ForReg que es el rezago

###########################################################

#############
# GRAFICO 1 #
#############

#Grafico de ingreso total personal neto por años de escolaridad segun zona
datos_jc %>% ggplot(aes(x = Escolari, y = itpn, group = ZONA)) +
  geom_hex(alpha = 0.8) + 
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  facet_wrap(~ZONA) +
  labs(title = "Gráfico 1. \nRelación entre años de escolaridad e ingreso personal neto",
       subtitle = "Distribución del ingreso según zona urbana y rural en Costa Rica",
       x = "Escolaridad",
       y = "Ingreso",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_y_log10(labels = label_number()) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        plot.caption = element_text(size = 6))
  
ggsave("../Manejo_de_Datos/graphs/Grafico1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 10, # Tamaño: 10 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

       
#############
# GRAFICO 2 #
#############

#Grafico de ingreso total personal neto por años de escolaridad segun region de planificacion 

datos_jc$REGION <- as_factor(datos_jc$REGION) 

datos_jc %>% ggplot(aes(x = Escolari, y = itpn, group = REGION, colour = REGION)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~REGION) +
  labs(title = "Gráfico 2. \nRelación entre escolaridad e ingreso personal neto según región de planificación",
       subtitle = "Comparativa de ingresos por años de escolaridad en las distintas regiones de planificación de Costa Rica",
       x = "Escolaridad",
       y = "Ingreso",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_y_log10(labels = label_number()) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        plot.caption = element_text(size = 6))

ggsave("../Manejo_de_Datos/graphs/Grafico2.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 10, # Tamaño: 10 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 3 #
#############

#Grafico de pobreza segun nivel de instrucción
datos_jc %>% filter(!is.na(np)) %>% 
  ggplot(aes(x = np, fill = NivInst)) +
  geom_bar(position = "fill") +
  labs(title = "Grafico 3. \nDistribución de la Pobreza según Nivel de Instrucción en Costa Rica",
       subtitle = "Comparación entre Pobreza Extrema, Pobreza No Extrema y No Pobreza según el Nivel Educativo",
       x = "Nivel de pobreza",
       y = NULL, # Eliminar el título del eje y
       fill = "Nivel de instrucción",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+  
  scale_fill_brewer(palette = "GnBu") +  # Paleta de colores 
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0))  # Cambiar el tamaño del texto del eje x

ggsave("../Manejo_de_Datos/graphs/Grafico3.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 10, # Tamaño: 10 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 4 #
#############

#Grafico de pobreza multidimensional segun nivel de instruccion
datos_jc %>% filter(!is.na(IPM_Pobreza)) %>% 
  ggplot(aes(x = IPM_Pobreza, fill = NivInst)) +
  geom_bar(position = "fill") +
  labs(title = "Gráfico 4. \nDistribución de la Pobreza Multidimensional según Nivel de Instrucción en Costa Rica",
       subtitle = "Comparación entre Pobreza Multidimensional y No Pobreza Multidimensional por Nivel Educativo en 2023",
       x = "Nivel de pobreza",
       y = NULL, # Eliminar el título del eje y
       fill = "Nivel de instrucción",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+  
  scale_fill_brewer(palette = "Greens") +  # Paleta de colores 
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0))  # Cambiar el tamaño del texto del eje x

ggsave("../Manejo_de_Datos/graphs/Grafico4.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 10, # Tamaño: 10 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 5.1 #
#############

#Numero de titulos obtenidos segun quintil de ingreso per capita
datos_jc %>% filter(!is.na(A16B)) %>%
  ggplot(aes(x = Q_IPCN, fill = A16B)) +
  geom_bar(position = "dodge") +  # Colocar barras separadas dentro de cada cuartil
  labs(title = "Gráfico 5.1. \nDistribución de Títulos Obtenidos según Quintil de Ingreso Per Cápita en Costa Rica",
       subtitle = "Cantidad de títulos obtenidos en cada nivel educativo, clasificados por quintiles de ingreso per cápita al 2023",
       x = NULL,  # Eliminar el título del eje x
       y = NULL,  # Eliminar el título del eje y
       fill = "Título obtenido",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+  # Título de la leyenda
  coord_polar() +  # Gráfico circular
  scale_y_log10() +  # Aplicar la transformación de raíz cuadrada a la escala de y
  scale_fill_brewer(palette = "Set3") +  # Paleta de colores 
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),  # Tamaño de texto de la leyenda
        axis.text.x = element_text(size = 8, hjust = 5),
        plot.caption = element_text(size = 6, hjust = 0))  # Tamaño del texto en el eje x

ggsave("../Manejo_de_Datos/graphs/Grafico5.1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 12, # Tamaño: 12 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 5.2 #
#############

datos_jc %>% 
  filter(!is.na(A16B) & !is.na(Q_IPCN) & A16B != "Ignorado") %>%  # Filtrar valores NA
  group_by(A16B, Q_IPCN) %>%  # Agrupar por título y quintil
  summarise(conteo = n()) %>%  # Contar el número de personas en cada grupo
  pivot_wider(names_from = Q_IPCN, values_from = conteo, values_fill = 0) %>% #view() # Convertir a formato ancho
  ggparcoord(columns = 2:6, groupColumn = 1, scale = "uniminmax") +
    labs(title = "Gráfico 5.2. \nDistribución de títulos académicos obtenidos por quintil de ingreso per cápita Costa Rica, 2023",
         subtitle = "Relación entre nivel educativo y nivel de ingreso per cápita según quintiles de ingresos",
         x = "Quintil de ingreso per cápita",
         y = NULL,
         caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
    scale_color_discrete(name = "Título") +
    scale_x_discrete(labels = c("Q1: 110683 ó menos" = "1", 
                              "Q2: Más de 110683 a 195000" = "2", 
                              "Q3: Más de 195000 a 321523" = "3", 
                              "Q4: Más de 321523 a 574085" = "4", 
                              "Q5: Más de 574085" = "5")) +
    cowplot::theme_cowplot() +
    theme(legend.text = element_text(size = 8),
          plot.caption = element_text(size = 6, hjust = 0),
          axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("../Manejo_de_Datos/graphs/Grafico5.2.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 14, # Tamaño: 14 pulgadas de ancho
       height = 8, # Tamaño: 8 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 5.3 #
#############

datos_jc %>% 
  filter(!is.na(A16B) & !is.na(Q_IPCN) & A16B != "Ignorado") %>%  # Filtrar valores NA
  group_by(A16B, Q_IPCN) %>%  # Agrupar por título y quintil
  summarise(conteo = n()) %>%  # Contar el número de personas en cada grupo
  pivot_wider(names_from = Q_IPCN, values_from = conteo, values_fill = 0) %>% #view() # Convertir a formato ancho
  ggparcoord(columns = 2:6, groupColumn = 1, scale = "globalminmax") +
  labs(title = "Gráfico 5.3. \nNúmero de títulos académicos obtenidos por quintil de ingreso per cápita Costa Rica, 2023",
       subtitle = "Comparación de la cantidad de títulos académicos obtenidos según quintiles de ingreso per cápita",
       x = "Quintil de ingreso per cápita",
       y = "Cantidad de títulos obtenidos",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_color_discrete(name = "Título") +
  scale_x_discrete(labels = c("Q1: 110683 ó menos" = "1", 
                              "Q2: Más de 110683 a 195000" = "2", 
                              "Q3: Más de 195000 a 321523" = "3", 
                              "Q4: Más de 321523 a 574085" = "4", 
                              "Q5: Más de 574085" = "5")) +
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("../Manejo_de_Datos/graphs/Grafico5.3.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 14, # Tamaño: 14 pulgadas de ancho
       height = 8, # Tamaño: 8 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 6.1 #
#############

#Grafico de distribucion del ingreso segun dominio de un segundo idioma
datos_jc %>% 
  ggplot(aes(x = A22A, y = itpn, fill = A22A)) +
  geom_boxplot() +  # Omitir valores extremos
  geom_hline(yintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", x = 0.5, y = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  scale_y_log10(labels = scales::comma) +  scale_fill_manual(values = c("#f7a072", "#0fa3b1")) +
  labs(title = "Gráfico 6.1. \nDistribución del ingreso total neto por persona según el dominio de un segundo idioma en Costa Rica",
       subtitle = "Comparación entre personas que dominan y no dominan un segundo idioma al 2023",
       x = "Dominio de un segundo idioma",
       y = "Ingreso total personal neto",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        plot.caption = element_text(size = 6, hjust = 0))

ggsave("../Manejo_de_Datos/graphs/Grafico6.1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 14, # Tamaño: 14 pulgadas de ancho
       height = 8, # Tamaño: 8 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 6.2 #
#############

datos_jc %>% 
  ggplot(aes(x = A22A, y = itpn, fill = A22A)) +
  geom_violin(outlier.shape = NA) +  # Omitir valores extremos
  geom_hline(yintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", x = 0.5, y = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("#22577a", "#57cc99")) +
  labs(title = "Gráfico 6.2. \nDistribución del ingreso total personal neto según dominio de un segundo idioma en Costa Rica",
       subtitle = "Comparación de la distribución de ingresos entre personas que dominan o no un segundo idioma en 2023",
       x = "Dominio de un segundo idioma",
       y = "Ingreso total personal neto",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        plot.caption = element_text(size = 6, hjust = 0))

ggsave("../Manejo_de_Datos/graphs/Grafico6.2.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 12, # Tamaño: 12 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada


#############
# GRAFICO 7 #
#############

#Grafico de ingreso segun formacion educativa forma

datos_jc %>% 
  filter(!is.na(ForReg) & itpn > 0 & ForReg != "Ignorada" & ForReg != "No definida") %>%  # Filtrar valores NA y menores o iguales a 0
  ggplot(aes(x = itpn, y = ForReg, fill = ForReg, color = ForReg)) +
  geom_density_ridges(alpha = 0.5) +
  geom_vline(xintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", y = 0.7, x = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  labs(title = "Gráfico 7. \nDistribución del ingreso total personal neto según formación educativa en Costa Rica",
       subtitle = "Comparación entre los niveles de formación educativa y el ingreso en 2023",
       x = "Ingreso",
       y = "Formación educativa",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_x_log10(labels = scales::comma) +  # Mostrar en millones
  scale_fill_brewer(palette = "Dark2") +  # Paleta cualitativa ordinal
  scale_color_brewer(palette = "Dark2") +  # Paleta cualitativa ordinal
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, angle = 25, hjust = 1),
        plot.caption = element_text(size = 6, hjust = 0))

ggsave("../Manejo_de_Datos/graphs/Grafico7.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 12, # Tamaño: 12 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 8 #
#############
  
#Graficos de porcentaje de pobreza segun si se tiene secundaria o primaria completa
datos_jc %>% filter(!is.na(np))%>% 
  ggplot(aes(x = Tiene_prim_completa, fill = np)) +
  geom_bar(position = "fill") +
  labs(title = "Gráfico 8. \nDistribución de la Pobreza Multidimensional por Nivel de Educación en Costa Rica",
       subtitle = "Análisis comparativo entre pobrezas según la completitud de primaria en 2023.",
       x = NULL,
       y = "Porcentaje",
       fill = "Nivel de pobreza",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_fill_brewer(palette = "OrRd") +  # Paleta de colores 
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0))

ggsave("../Manejo_de_Datos/graphs/Grafico8.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 10, # Tamaño: 10 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#############
# GRAFICO 9 #
#############

datos_jc %>% filter(!is.na(np))%>% 
  ggplot(aes(x = Tiene_sec_completa, fill = np)) +
  geom_bar(position = "fill") +
  labs(title = "Gráfico 9. \nDistribución de la Pobreza según Nivel de Educación Secundaria en Costa Rica",
       subtitle = "Comparación del porcentaje de pobreza extrema, pobreza no extrema y no pobreza según la finalización de la educación secundaria, 2023.",
       x = NULL,
       y = "Porcentaje",
       fill = "Nivel de pobreza",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_fill_brewer(palette = "YlOrBr") +  # Paleta de colores 
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 6, hjust = 0))

ggsave("../Manejo_de_Datos/graphs/Grafico9.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 10, # Tamaño: 10 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

##############
# GRAFICO 10 #
##############

datos_jc %>% filter(!is.na(np))%>% 
  ggplot(aes(x = Tiene_est_postsec, fill = np)) +
  geom_bar(position = "fill") +
  labs(title = "Gráfico 10. \nDistribución de la Pobreza según Nivel de Educación Postsecundaria en Costa Rica",
       subtitle = "Comparación del porcentaje de pobreza extrema, pobreza no extrema y no pobreza según la finalización de estudios postsecundarios, 2023.",
       x = NULL,
       y = "Porcentaje",
       fill = "Nivel de pobreza",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_fill_brewer(palette = "Purples") +  # Paleta de colores 
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(size = 9),
        plot.caption = element_text(size = 6, hjust = 0))

ggsave("../Manejo_de_Datos/graphs/Grafico10.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 10, # Tamaño: 10 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

##############
# GRAFICO 11.1 #
##############

#Grafico de distribucion de ingreso por universidad publica

datos_jc %>% filter(!is.na(A15B) & A15B != "Ignorado")%>% 
  ggplot(aes(x = A15B, y = itpn, fill = A15B)) +
  geom_violin() +
  geom_hline(yintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", x = 1.15, y = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  labs(title = "Gráfico 11.1 \nDistribución del Ingreso Neto Total por Persona según Universidad Pública en Costa Rica",
       subtitle = "Comparación del ingreso total por persona neto entre egresados de universidades públicas, considerando la línea de pobreza en 2023.",
       x = "Universidad",
       y = "Ingreso total por persona neto",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_y_log10(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, angle = 25, hjust = 1),
        plot.caption = element_text(size = 6, hjust = 0))

ggsave("../Manejo_de_Datos/graphs/Grafico11.1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.5, # Tamaño: 11.5 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

##############
# GRAFICO 11.2 #
##############

datos_jc %>% filter(!is.na(A15B) & A15B != "Ignorado" & itpn > 0)%>% 
  ggplot(aes(x = itpn, y = A15B, fill = A15B)) +
  geom_density_ridges() +
  geom_vline(xintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", y = 0.7, x = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  labs(title = "Gráfico 11.2. \nDistribución del Ingreso Neto Total por Persona según Universidad Pública en Costa Rica",
       subtitle = "Comparación del ingreso total por persona neto entre egresados de universidades públicas, considerando la línea de pobreza en 2023.",
       x = "Universidad",
       y = "Ingreso total por persona neto",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  scale_x_log10(labels = scales::comma) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, angle = 25, hjust = 1),
        plot.caption = element_text(size = 6, hjust = 0))

ggsave("../Manejo_de_Datos/graphs/Grafico11.2.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.5, # Tamaño: 11.5 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#Grafico mapa de regiones de planificacion

# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("https://admin.inec.cr/sites/default/files/2023-07/UGER_MGN_2022.zip", destfile = "data/world_shape_file.zip")
# You now have it in your current working directory, have a look!

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
unzip("data/world_shape_file.zip", junkpaths = FALSE)
#  -- > You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)

my_sf <- read_sf("UGER_MGN_2022.shp")

ggplot(my_sf) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()

####################################################################

#######################################################
#Gráficos relacionados con el tipo de centro educativo#
#######################################################

#Grafico de nivel de pobreza segun tipo de centro educativo al que asistio
graf_np_vs_tipo_colegio <- as.data.frame(table(datos_jc$np, datos_jc$A15A)) %>% filter(Var2 != "Ignorado")
graf_np_vs_tipo_colegio %>% ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_col(position = "fill") +
  labs(x = "Tipo de centro educativo al que asistió",
       title = "Gráfico # \nNivel de pobreza según tipo de\ncentro educativo al que se asistió",
       y = "",
       fill = "Nivel de pobreza",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(size = 8, angle = 20, hjust = 1),
        plot.caption = element_text(size = 6, hjust = 0))

#Graficos de distribucion del itpn segun centro educativo al que se asistio
datos_jc %>% filter(A15A != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15A, itpn, median), fill = A15A)) +
  geom_density_ridges() +
  labs(title = "Grafico #\nDistribución del ingreso total neto por persona\nsegún tipo de centro educativo al que se asistió",
       x = "Ingreso total neto por persona",
       y = "Tipo de centro educativo",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_x_log10(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, angle = 25, hjust = 1),
        plot.caption = element_text(size = 6, hjust = 0))

datos_jc %>% filter(A15A != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15A, itpn, median), fill = A15A)) +
  geom_boxplot() +
  labs(title = "Grafico #\nDistribución del ingreso total neto por persona\nsegún tipo de centro educativo al que se asistió",
       x = "Ingreso total neto por persona",
       y = "Tipo de centro educativo",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_x_log10(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, angle = 25, hjust = 1),
        plot.caption = element_text(size = 6, hjust = 0))


#Grafico de evolucion de la distribucion del tipo de centro educativo al que se asistio segun quintil de ingreso per capita neto (version ultra wow)
as.data.frame(prop.table(table(datos_jc$A15A, datos_jc$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  # Gráfico de área con proporciones
  geom_area(position = "fill", alpha = 0.8, color = "white", size = 0.3) +
  # Etiquetas y títulos mejorados
  labs(
    title = "Gráfico #.\nDistribución de tipo de centro educativo asistido por quintil de ingreso per cápita",
    subtitle = "Relación entre tipo de centro educativo y nivel de ingreso per cápita (Costa Rica, 2023)",
    x = "Quintil de ingreso per cápita",
    y = "Proporción acumulada",
    fill = "Tipo de centro educativo",
    caption = "Fuente: INEC, Encuesta Nacional de Hogares 2023"
  ) +
  # Paleta de colores con viridis para mayor claridad
  scale_fill_viridis_d(option = "C") +
  # Ejes bien formateados
  scale_x_continuous(
    breaks = 1:5, 
    labels = c("Q1: 110683 ó menos", "Q2: 110683-195000", "Q3: 195000-321523", "Q4: 321523-574085", "Q5: Más de 574085")
  ) +
  # Estilo minimalista y limpio
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",  # Mover la leyenda arriba
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )

###############################################
#Gráficos relacionados con universidad pública#
###############################################
 
#Grafico de nivel de pobreza segun la universidad publica a la que se asistio
tab_np_vs_univ %>% 
  pivot_longer(cols = UCR:UTN,
               names_to = "Universidad",
               values_to = "Proporcion") %>% 
  mutate(Proporcion = Proporcion / 100) %>% 
  ggplot(aes(x = fct_relevel(Universidad, c("UNED", "UTN", "UNA", "UCR", "TEC")), y = Proporcion, fill = Nivel_de_pobreza)) +
  geom_col() +
  labs(x = "Universidad pública a la que se asistió",
       title = "Gráfico # \nNivel de pobreza según universidad pública a la que se asistió",
       y = "",
       fill = "Nivel de pobreza",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0))

#Grafico de la cantidad de personas pobres que fueron a una universidad publica, por universidad segun su formacion educativa formal
datos_jc %>% 
  filter(!is.na(A15B) & !is.na(ForReg) & np != "No pobre" & A15B != "Ignorado") %>% 
  ggplot(aes(x = fct_infreq(A15B), fill = fct_infreq(ForReg))) +
    geom_bar() +
  labs(title = "Gráfico #\nCantidad de personas en condición de pobreza por universidad",
       subtitle = "según formación educativa formal",
       x = "Universidad pública a la que se asistió",
       y = "Cantidad de personas en condición de pobreza",
       fill = "Formación educativa formal",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))

#Grafico de la proporcion de personas no pobres que fueron a universidad publica, por universidad segun formacion educativa formal
datos_jc %>% 
  filter(!is.na(A15B) & !is.na(ForReg) & np == "No pobre" & A15B != "Ignorado") %>% 
  ggplot(aes(x = fct_infreq(A15B), fill = fct_infreq(ForReg))) +
  geom_bar() +
  labs(title = "Gráfico #\nCantidad de personas en condición de pobreza por universidad",
       subtitle = "según formación educativa formal",
       x = "Universidad pública a la que se asistió",
       y = "Cantidad de personas en condición de no pobreza",
       fill = "Formación educativa formal",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))


#Graficos de distribucion del itpn segun universidad publica a la que se asistio
datos_jc %>% filter(A15B != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15B, itpn, median), fill = A15B)) +
  geom_density_ridges() +
  labs(title = "Grafico #\nDistribución del ingreso total neto por persona\nsegún universidad pública a la que se asistió",
       x = "Ingreso total neto por persona",
       y = "Universidad pública a la que se asistió",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_x_log10(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, hjust = 1),
        plot.caption = element_text(size = 6, hjust = 0))

datos_jc %>% filter(A15B != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15B, itpn, median), fill = A15B)) +
  geom_boxplot() +
  labs(title = "Grafico #\nDistribución del ingreso total neto por persona\nsegún universidad pública a la que se asistió",
       x = "Ingreso total neto por persona",
       y = "Universidad pública a la que se asistió",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_x_log10(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, hjust = 1),
        plot.caption = element_text(size = 6, hjust = 0),
        axis.title.y = element_text(size = 12))

################################################
#Gráficos relacionados con nivel de instruccion#
################################################

#Grafico de evolucion de nivel de pobreza segun nivel de instruccion

as.data.frame(table(datos_jc$np, fct_collapse(datos_jc$NivInst, 
                                              "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                              "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                              "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta")))) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(NivInst = Var2) %>% 
  ggplot(aes(x = NivInst, y = Freq, fill = Nivel_de_pobreza)) + 
  geom_col(position = "fill") +
  labs(title = "Gráfico #.\nNivel de pobreza según nivel de instrucción",
     subtitle = "en porcentajes",
     x = "Nivel de instrucción",
     y = NULL,
     fill = "Nivel de pobreza",
     caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 0, size = 8, angle = -20))

#Grafico de la evolucion de la estabilidad laboral segun nivel de instruccion
as.data.frame(prop.table(table(datos_jc$Estabili, fct_collapse(datos_jc$NivInst, 
                                              "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                              "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                              "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta"))), margin = 2)) %>% 
  rename(Estabilidad = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(NivInst = Var2) %>% 
  filter(Estabilidad == "Empleo permanente") %>% 
  ggplot(aes(x = NivInst, y = Freq, color = Estabilidad, group = Estabilidad)) +
    geom_line(color = "blue") +
  labs(title = "Gráfico #.\nPorcentaje de personas con empleo permanente por nivel de instrucción",
     x = "Nivel de instrucción",
     y = NULL,
     caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 0, size = 8, angle = -20))

#Grafico de porcentaje de personas satisfechas con su trabajo por nivel de instruccion
as.data.frame(prop.table(table(datos_jc$InsLab, fct_collapse(datos_jc$NivInst, 
                                                               "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                                               "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                                               "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta"))), margin = 2)) %>% 
  rename(Satisfaccion = Var1) %>% 
  filter(Var2 != "Satisfacción ignorada") %>% 
  rename(NivInst = Var2) %>% 
  filter(Satisfaccion == "Satisfecho", NivInst != "Ignorado") %>% 
  ggplot(aes(x = NivInst, y = Freq, color = Satisfaccion, group = Satisfaccion)) +
  geom_line(color = "red") +
  labs(title = "Gráfico #.\nPorcentaje de personas satisfechas con su empleo por nivel de instrucción",
       x = "Nivel de instrucción",
       y = NULL,
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 0, size = 8, angle = -20))

#Grafico: distribucion del itpn por nivel de instruccion (version ultra mejorada)
datos_jc %>% 
  filter(NivInst != "Ignorado" & itpn > 0) %>%
  mutate(NivInst = fct_collapse(NivInst,
                                "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta"))) %>%
  ggplot(aes(x = itpn, y = fct_reorder(NivInst, itpn, median), fill = NivInst)) +
  # Mejorar la visualización con densidades y lineas más detalladas
  geom_density_ridges(rel_min_height = 0.01, scale = 1.2, alpha = 0.8, color = "white") + 
  # Etiquetas y títulos mejorados
  labs(
    title = "Grafico #.\nDistribución del ingreso total neto por persona según nivel de instrucción",
    subtitle = "Costa Rica, 2023",
    x = "Ingreso total neto por persona (log10)",
    y = "Nivel de instrucción",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  # Ajuste de escala con formato de coma en el eje x
  scale_x_log10(labels = scales::comma) +
  # Mejora de la paleta de colores con gradientes suaves
  scale_fill_viridis_d(option = "C") +
  # Temas mejorados con mayor atención a los detalles visuales
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Ocultar leyenda
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )

#Grafico evolucion de la distribucion del nivel de instruccion por quintil de ingreso 
as.data.frame(prop.table(table(fct_collapse(datos_jc$NivInst, 
                                            "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                            "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                            "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta")), 
                               datos_jc$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_area() +
  labs(title = "Gráfico #. \nDistribución del nivel de instrucción por quintil de ingreso per cápita Costa Rica, 2023",
       subtitle = "Relación entre el nivel de instrucción y nivel de ingreso per cápita según quintiles de ingresos",
       x = "Quintil de ingreso per cápita",
       y = NULL,
       fill = "Nivel de instrucción",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Spectral") +
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 1))

#######################################
#Gráficos relacionados con escolaridad#
#######################################

#Grafico correlacion entre años de escolaridad e ingreso total por persona neto

datos_jc %>% 
  ggplot(aes(x = Escolari, y = itpn, color = ZONA)) +
  # Cambiar a puntos con transparencia para evitar solapamientos y tamaño adecuado
  geom_point(alpha = 0.6, size = 2) +
  # Línea de regresión más destacada, con mayor grosor
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  # Facetas según la zona
  facet_wrap(~ZONA) +
  # Etiquetas y títulos mejorados
  labs(title = "Gráfico #. \nRelación entre años de escolaridad e ingreso personal neto",
       subtitle = "Distribución del ingreso según zona urbana y rural en Costa Rica",
       x = "Escolaridad (Años)",
       y = "Ingreso Personal Neto (₡, log10)",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  # Escala logarítmica en el eje Y con etiquetas más claras y separadores de miles
  scale_y_log10(labels = label_number(scale = 1, big.mark = ",")) +
  # Colores diferenciados para las zonas
  scale_color_brewer(palette = "Set1", name = "Zona") +
  # Aplicar un tema profesional de cowplot con algunos ajustes personalizados
  cowplot::theme_cowplot() +
  theme(
    # Ajustar la posición y el tamaño del título y subtítulo
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    # Redimensionar y ajustar las etiquetas de los ejes
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    # Ajustes en la leyenda y el pie de página
    legend.position = "none",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.caption = element_text(size = 7, hjust = 0)
  )

#Grafico correlacion entre años de escolaridad e intensidad de la pobreza multidimensional

datos_jc %>% 
  ggplot(aes(x = Escolari, y = IPM_Intensidad, color = ZONA)) +
  # Cambiar a puntos con transparencia para evitar solapamientos y tamaño adecuado
  geom_point(alpha = 0.6, size = 2) +
  # Línea de regresión más destacada, con mayor grosor
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  # Facetas según la zona
  facet_wrap(~ZONA) +
  # Etiquetas y títulos mejorados
  labs(title = "Gráfico #. \nRelación entre años de escolaridad y la intensidad de la pobreza",
       x = "Escolaridad (Años)",
       y = "Intensidad de la pobreza",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  # Escala logarítmica en el eje Y con etiquetas más claras y separadores de miles
  scale_y_log10(labels = label_number(scale = 1, big.mark = ",")) +
  # Colores diferenciados para las zonas
  scale_color_brewer(palette = "Set1", name = "Zona") +
  # Aplicar un tema profesional de cowplot con algunos ajustes personalizados
  cowplot::theme_cowplot() +
  theme(
    # Ajustar la posición y el tamaño del título y subtítulo
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    # Redimensionar y ajustar las etiquetas de los ejes
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    # Ajustes en la leyenda y el pie de página
    legend.position = "none",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.caption = element_text(size = 7, hjust = 0)
  )

###########################################
#Gráficos relacionados con título obtenido#
###########################################

#Grafico de evolucion de nivel de pobreza segun titulo

as.data.frame(table(datos_jc$np, datos_jc$A16B)) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(titulo = Var2) %>% 
  ggplot(aes(x = titulo, y = Freq, fill = Nivel_de_pobreza)) + 
  geom_col(position = "fill") +
  labs(title = "Gráfico #.\nNivel de pobreza según título obtenido",
       subtitle = "en porcentajes",
       x = "Título",
       y = NULL,
       fill = "Nivel de pobreza",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 0, size = 8, angle = -20))

#Grafico de la evolucion de la estabilidad laboral segun titulo obtenido
as.data.frame(prop.table(table(datos_jc$Estabili, datos_jc$A16B), margin = 2)) %>% 
  rename(Estabilidad = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(titulo = Var2) %>% 
  filter(Estabilidad == "Empleo permanente") %>% 
  ggplot(aes(x = titulo, y = Freq, color = Estabilidad, group = Estabilidad)) +
  geom_line(color = "blue") +
  labs(title = "Gráfico #.\nPorcentaje de personas con empleo permanente según título obtenido",
       x = "Título",
       y = NULL,
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 0, size = 8, angle = -20))

#Grafico de porcentaje de personas satisfechas con su trabajo por el titulo obtenido
as.data.frame(prop.table(table(datos_jc$InsLab, datos_jc$A16B), margin = 2)) %>% 
  rename(Satisfaccion = Var1) %>% 
  filter(Var2 != "Satisfacción ignorada") %>% 
  rename(titulo = Var2) %>% 
  filter(Satisfaccion == "Satisfecho", titulo != "Ignorado") %>% 
  ggplot(aes(x = titulo, y = Freq, color = Satisfaccion, group = Satisfaccion)) +
  geom_line(color = "red") +
  labs(title = "Gráfico #.\nPorcentaje de personas satisfechas con su empleo según título obtenido",
       x = "Título",
       y = NULL,
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 0, size = 8, angle = -20))

#Grafico: distribucion del itpn por título obtenido (version ultra mejorada)
datos_jc %>% 
  filter(A16B != "Ignorado" & itpn > 0) %>%
  ggplot(aes(x = itpn, y = fct_reorder(A16B, itpn, median), fill = A16B)) +
  # Mejorar la visualización con densidades y lineas más detalladas
  geom_density_ridges(rel_min_height = 0.01, scale = 1.2, alpha = 0.8, color = "white") + 
  # Etiquetas y títulos mejorados
  labs(
    title = "Grafico #.\nDistribución del ingreso total neto por persona según título obtenido",
    subtitle = "Costa Rica, 2023",
    x = "Ingreso total neto por persona (log10)",
    y = "Título obtenido",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  # Ajuste de escala con formato de coma en el eje x
  scale_x_log10(labels = scales::comma) +
  # Mejora de la paleta de colores con gradientes suaves
  scale_fill_viridis_d(option = "C") +
  # Temas mejorados con mayor atención a los detalles visuales
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Ocultar leyenda
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )

datos_jc %>% 
  filter(A16B != "Ignorado" & itpn > 0) %>%
  ggplot(aes(x = itpn, y = fct_reorder(A16B, itpn, median), fill = A16B)) +
  # Boxplot con ajuste de grosor y transparencia
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, outlier.fill = "white", alpha = 0.8) +
  # Títulos mejorados con mayor tamaño y alineación
  labs(title = "Gráfico #\nDistribución del ingreso total neto por persona según título obtenido",
       subtitle = "Relación entre ingreso total neto y el tipo de título académico alcanzado",
       x = "Ingreso total neto por persona (₡, escala logarítmica)",
       y = "Tipo de título académico",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  # Escala logarítmica en el eje X con separadores de miles
  scale_x_log10(labels = scales::comma) +
  # Mejora de la paleta de colores
  scale_fill_brewer(palette = "Set2") +
  # Aplicación de un tema de cowplot con personalización
  cowplot::theme_cowplot() +
  theme(
    # Título y subtítulo con mayor tamaño y centrado
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    # Ajustes de etiquetas de los ejes
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9, angle = 20, hjust = 1),
    # Estilo del pie de página
    plot.caption = element_text(size = 7, hjust = 0),
    # Ocultar leyenda
    legend.position = "none"
  )

#Grafico evolucion de la distribucion de los títulos obtenidos por quintil de ingreso 
as.data.frame(prop.table(table(datos_jc$A16B,datos_jc$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_area() +
  labs(title = "Gráfico #. \nDistribución de títulos obtenidos por quintil de ingreso per cápita Costa Rica, 2023",
       x = "Quintil de ingreso per cápita",
       y = NULL,
       fill = "Título",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Spectral") +
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 1))

#####################################################
#Gráficos relacionados con dominio de segundo idioma#
#####################################################

#Grafico evolucion de la distribucion de las personas que dominan un segundo idioma por quintil de ingreso 
as.data.frame(prop.table(table(datos_jc$A22A,datos_jc$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_area() +
  labs(title = "Gráfico #. \nDistribución de personas que dominan un segundo idioma por quintil de ingreso per cápita Costa Rica, 2023",
       x = "Quintil de ingreso per cápita",
       y = NULL,
       fill = "Dominio de un segundo idioma",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Spectral") +
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 1))

#Grafico: distribucion del itpn segun dominio de segundo idioma (version ultra mejorada)
datos_jc %>% 
  filter(A22A != "Ignorado" & itpn > 0) %>%
  ggplot(aes(x = itpn, y = fct_reorder(A22A, itpn, median), fill = A22A)) +
  # Mejorar la visualización con densidades y lineas más detalladas
  geom_density_ridges(rel_min_height = 0.01, scale = 1.2, alpha = 0.8, color = "white") + 
  # Etiquetas y títulos mejorados
  labs(
    title = "Grafico #.\nDistribución del ingreso total neto por persona según dominio de un segundo idioma",
    subtitle = "Costa Rica, 2023",
    x = "Ingreso total neto por persona (log10)",
    y = "Dominio de un segundo idioma",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  # Ajuste de escala con formato de coma en el eje x
  scale_x_log10(labels = scales::comma) +
  # Mejora de la paleta de colores con gradientes suaves
  scale_fill_viridis_d(option = "C") +
  # Temas mejorados con mayor atención a los detalles visuales
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Ocultar leyenda
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )





