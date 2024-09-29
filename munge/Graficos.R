
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
library(sf)

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
                     geom_violin() +
                     scale_y_log10(labels = label_number())

## USAR ForReg que es el rezago

###########################################################

#Grafico de ingreso total personal neto por años de escolaridad segun zona
datos_jc %>% ggplot(aes(x = Escolari, y = itpn, group = ZONA)) +
  geom_hex() + 
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  facet_wrap(~ZONA) +
  labs(title = "Ingreso total personal neto\npor años de escolaridad",
       subtitle = "según zona",
       x = "Escolaridad",
       y = "Ingreso") +
  scale_y_log10(labels = label_number()) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none")
  
#Grafico de ingreso total personal neto por años de escolaridad segun region de planificacion 

datos_jc$REGION <- as_factor(datos_jc$REGION) 

datos_jc %>% ggplot(aes(x = Escolari, y = itpn, group = REGION, colour = REGION)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~REGION) +
  labs(title = "Ingreso total personal neto\npor años de escolaridad",
       subtitle = "según región de planificación",
       x = "Escolaridad",
       y = "Ingreso") +
  scale_y_log10(labels = label_number()) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none")

#Grafico de pobreza segun nivel de instrucción
datos_jc %>% filter(!is.na(np)) %>% 
  ggplot(aes(x = np, fill = NivInst)) +
  geom_bar(position = "fill") +
  labs(title = "Nivel de pobreza",
       subtitle = "según nivel de instrucción",
       x = "Nivel de pobreza",
       y = NULL, # Eliminar el título del eje y
       fill = "Nivel de instrucción") +  
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8))  # Cambiar el tamaño del texto del eje x

#Grafico de pobreza multidimensional segun nivel de instruccion
datos_jc %>% filter(!is.na(IPM_Pobreza)) %>% 
  ggplot(aes(x = IPM_Pobreza, fill = NivInst)) +
  geom_bar(position = "fill") +
  labs(title = "Nivel de pobreza",
       subtitle = "según nivel de instrucción",
       x = "Nivel de pobreza",
       y = NULL, # Eliminar el título del eje y
       fill = "Nivel de instrucción") +  
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8))  # Cambiar el tamaño del texto del eje x

#Numero de titulos obtenidos segun quintil de ingreso per capita
datos_jc %>% filter(!is.na(A16B)) %>%
  ggplot(aes(x = Q_IPCN, fill = A16B)) +
  geom_bar(position = "dodge") +  # Colocar barras separadas dentro de cada cuartil
  labs(title = "Número de títulos obtenidos",
       subtitle = "Según quintil de ingreso per cápita",
       x = NULL,  # Eliminar el título del eje x
       y = NULL,  # Eliminar el título del eje y
       fill = "Título obtenido") +  # Título de la leyenda
  coord_polar() +  # Gráfico circular
  scale_y_log10() +  # Aplicar la transformación de raíz cuadrada a la escala de y
  cowplot::theme_cowplot() +
  theme(legend.text = element_text(size = 8),  # Tamaño de texto de la leyenda
        axis.text.x = element_text(size = 8))  # Tamaño del texto en el eje x


#Grafico de distribucion del ingreso segun dominio de un segundo idioma
datos_jc %>% 
  ggplot(aes(x = A22A, y = itpn, fill = A22A)) +
  geom_boxplot() +  # Omitir valores extremos
  geom_hline(yintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", x = 0.5, y = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  scale_y_log10(labels = scales::comma) +  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Distribución del ingreso total personal neto",
       subtitle = "según dominio de un segundo idioma",
       x = "Dominio de un segundo idioma",
       y = "Ingreso total personal neto") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none")

datos_jc %>% 
  ggplot(aes(x = A22A, y = itpn, fill = A22A)) +
  geom_violin(outlier.shape = NA) +  # Omitir valores extremos
  geom_hline(yintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", x = 0.5, y = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("pink", "blue")) +
  labs(title = "Distribución del ingreso total personal neto",
       subtitle = "según dominio de un segundo idioma",
       x = "Dominio de un segundo idioma",
       y = "Ingreso total personal neto") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none")


#Grafico de ingreso segun formacion educativa forma

datos_jc %>% 
  filter(!is.na(ForReg) & itpn > 0 & ForReg != "Ignorada" & ForReg != "No definida") %>%  # Filtrar valores NA y menores o iguales a 0
  ggplot(aes(x = itpn, y = ForReg, fill = ForReg, color = ForReg)) +
  geom_density_ridges(alpha = 0.5) +
  geom_vline(xintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", y = 0.7, x = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  labs(title = "Ingreso total personal neto",
       subtitle = "según formación educativa formal",
       x = "Ingreso en millones de colones",
       y = "Formación educativa") +
  scale_x_log10(labels = scales::comma) +  # Mostrar en millones
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, angle = 25, hjust = 1))

  
#Graficos de porcentaje de pobreza segun si se tiene secundaria o primaria completa
datos_jc %>% filter(!is.na(np))%>% 
  ggplot(aes(x = Tiene_prim_completa, fill = np)) +
  geom_bar(position = "fill") +
  cowplot::theme_cowplot()

datos_jc %>% filter(!is.na(np))%>% 
  ggplot(aes(x = Tiene_sec_completa, fill = np)) +
  geom_bar(position = "fill") +
  cowplot::theme_cowplot()

#Grafico de distribucion de ingreso por universidad publica

datos_jc %>% filter(!is.na(A15B) & A15B != "Ignorado")%>% 
  ggplot(aes(x = A15B, y = itpn, fill = A15B)) +
  geom_violin() +
  geom_hline(yintercept = 129038, color = "red", linetype = "dashed", size = 1) +  # Línea horizontal
  annotate("text", x = 1.15, y = 150000, label = "Línea de pobreza", color = "black", size = 2, hjust = 0) +  # Texto
  labs(title = "Ingreso total por persona neto",
       subtitle = "según universidad pública a la que se asistió",
       x = "Universidad",
       y = "Ingreso total por persona neto") +
  scale_y_log10(labels = scales::comma) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, angle = 25, hjust = 1))






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


