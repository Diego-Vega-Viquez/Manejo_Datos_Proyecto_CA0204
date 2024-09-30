
# Acá van los códigos de algunos graficos que generemos

# Librerías:

install.packages("ProjectTemplate")
install.packages("devtools")
install.packages("tidyverse")
install.packages("cowplot")
install.packages("openintro")
install.packages("cowplot")
install.packages("ggridges")
install.packages("sf")

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
library(sf) library(RColorBrewer)

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
                     labs( title = "Gráfico 12. \nComparación del Ingreso Principal Neto del Hogar según Nivel de Instrucción, en Costa Rica al 2023",
                           subtitle = "Expresado en colones costarricenses",
                           x = "Nivel de Instrucción",
                           y = "Ingreso Principal Neto del Hogar",
                           fill =  "Nivel de Instrucción",
                           caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.")+
                     theme(axis.text.x = element_blank()) 

ggsave("../Manejo_de_Datos/graphs/Grafico12.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.5, # Tamaño: 11.5 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

variables_utiles %>% filter(spmn > 0 &!is.na(A16B)) %>% ggplot(aes(x = spmn, 
                                                     y = A16B, 
                                                     fill = A16B, 
                                                     color = A16B)) + 
                     geom_density_ridges(alpha = 0.5) + 
                     scale_x_log10(labels = label_number()) +
                      labs( title = "Figura 13. \nDistribución del Ingreso Total por persona del Hogar según Grado Académico en Costa Rica, 2023",
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
       width = 11.5, # Tamaño: 11.5 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

variables_utiles %>% filter(!is.na(Q_IPCN)) %>% 
                     ggplot(aes(x = Escolari, 
                                y = ipsnt, 
                                colour = Q_IPCN)) +
                      geom_jitter() +
                      geom_smooth(method = "lm", se = FALSE, colour = "black") +
                      facet_wrap(~Q_IPCN) +
  labs(title = "Figura 14. \nRelación entre los años de escolaridad e ingreso salario neto",
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
       width = 11.5, # Tamaño: 11.5 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

## USAR ForReg que es el rezago

###########################################################

#############
# GRAFICO 1 #
#############

#Grafico de ingreso total personal neto por años de escolaridad segun zona
datos_jc %>% ggplot(aes(x = Escolari, y = itpn, group = ZONA)) +
  geom_hex() + 
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
# GRAFICO 5 #
#############

#Numero de titulos obtenidos segun quintil de ingreso per capita
datos_jc %>% filter(!is.na(A16B)) %>%
  ggplot(aes(x = Q_IPCN, fill = A16B)) +
  geom_bar(position = "dodge") +  # Colocar barras separadas dentro de cada cuartil
  labs(title = "Gráfico 5. \nDistribución de Títulos Obtenidos según Quintil de Ingreso Per Cápita en Costa Rica",
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

ggsave("../Manejo_de_Datos/graphs/Grafico5.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 12, # Tamaño: 12 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
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
       width = 13, # Tamaño: 12 pulgadas de ancho
       height = 6, # Tamaño: 6 pulgadas de alto
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
# GRAFICO 11 #
##############

#Grafico de distribucion de ingreso por universidad publica

datos_jc %>% filter(!is.na(A15B) & A15B != "Ignorado")%>% 
  ggplot(aes(x = A15B, y = itpn, fill = A15B)) +
  geom_violin() +
  geom_hline(yintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", x = 1.15, y = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  labs(title = "Gráfico 11. \nDistribución del Ingreso Neto Total por Persona según Universidad Pública en Costa Rica",
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

ggsave("../Manejo_de_Datos/graphs/Grafico11.png", 
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

