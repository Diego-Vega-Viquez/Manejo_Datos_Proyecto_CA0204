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

#######################################################
#Gráficos relacionados con el tipo de centro educativo#
#######################################################

##### LISTO
# 1 -- Grafico de nivel de pobreza segun tipo de centro educativo al que asistio
#####
as.data.frame(table(ENAHO$np, ENAHO$A15A)) %>% 
  filter(Var2 != "Ignorado") %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_col(position = "fill", alpha = 0.8, color = "white", size = 0.3) +  # Gráfico de área
  labs(
    title = "Gráfico 1.\nNivel de pobreza según tipo de centro educativo al que se asistió",
    subtitle = "Relación entre tipo de centro educativo y nivel de pobreza (Costa Rica, 2023)",
    x = "Tipo de centro educativo al que asistió",
    y = "Proporción acumulada",
    fill = "Nivel de pobreza",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_fill_viridis_d(option = "F") +  # Paleta de colores viridis
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",  # Mover la leyenda arriba
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )

ggsave("../Manejo_de_Datos/graphs/Grafico1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 2 -- Graficos de distribucion del itpn segun centro educativo al que se asistio
#####
#####
# 2.1 -- Distribución del ingreso total neto por persona según tipo de centro educativo al que se asistió
#####
ENAHO %>% 
  filter(A15A != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15A, itpn, median), fill = A15A)) +
  geom_density_ridges(alpha = 0.8, color = "white", size = 0.3) +  # Ajuste en las curvas de densidad
  geom_vline(xintercept = 129038, color = "red", linetype = "dotted", size = 0.8) +  # Línea de pobreza mejorada
  annotate("text", y = 0.5, x = 180000, label = "Línea de pobreza", color = "red", 
           size = 3, hjust = 0, vjust = -0.5, fontface = "italic") +  # Texto mejorado
  labs(
    title = "Gráfico 2.1.\nDistribución del Ingreso Neto Personal por Tipo de Centro Educativo Asistido",
    subtitle = "Ingreso expresado en colones costarricenses",
    x = "Ingreso Personal Neto(log10)",
    y = "Tipo de centro educativo",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_x_log10(labels = scales::comma) +
  scale_fill_viridis_d(option = "C") +  # Paleta de colores viridis
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "none",  # Sin leyenda
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 10, color = "gray20"),  # Ajuste de texto en eje X
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico2.1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 2.2 --
#####
ENAHO %>% 
  filter(A15A != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15A, itpn, median), fill = A15A)) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, outlier.fill = "white", alpha = 0.8) +  # Boxplot con opacidad
  geom_vline(xintercept = 129038, color = "red", linetype = "dotted", size = 0.8) +  # Línea de pobreza
  annotate("text", y = 2.5, x = 15000, label = "Línea de pobreza", color = "red", 
           size = 3, hjust = 0, vjust = -0.5, fontface = "italic") +  # Texto mejorado
  labs(
    title = "Gráfico 2.2.\nDistribución del ingreso total neto por persona según tipo de centro educativo al que se asistió",
    subtitle = "Ingreso expresado en colones costarricenses",
    x = "Ingreso Personal Neto (log10)",
    y = "Tipo de centro educativo",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_x_log10(labels = scales::comma) +
  scale_fill_viridis_d(option = "C") +  # Paleta de colores viridis
  theme_minimal(base_size = 14) +  # Tema minimalista para variar
  theme(
    legend.position = "none",  # Sin leyenda
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 10, color = "gray20"),  # Ajuste de texto en eje X
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico2.2.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 3 -- Grafico de evolucion de la distribucion del tipo de centro educativo al que se asistio segun quintil de ingreso per capita neto (version ultra wow)
#####
as.data.frame(prop.table(table(ENAHO$A15A, ENAHO$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  # Gráfico de área con proporciones
  geom_area(position = "fill", alpha = 0.8, color = "white", size = 0.3) +
  # Etiquetas y títulos mejorados
  labs(
    title = "Gráfico 3.\nDistribución de tipo de centro educativo asistido por quintil de ingreso per cápita",
    subtitle = "Relación entre tipo de centro educativo y nivel de ingreso per cápita (Costa Rica, 2023)",
    x = "Quintil de ingreso per cápita",
    y = "Proporción acumulada",
    fill = "Tipo de centro educativo",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  # Paleta de colores con viridis para mayor claridad
  scale_fill_viridis_d(option = "C") +
  # # Ejes bien formateados
  # scale_x_continuous(
  #   breaks = 1:5, 
  #   labels = c("Q1: 110683 ó menos", "Q2: 110683-195000", "Q3: 195000-321523", "Q4: 321523-574085", "Q5: Más de 574085")
  # ) +
  # Estilo minimalista y limpio
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",  # Mover la leyenda arriba
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico3.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
###############################################
#Gráficos relacionados con universidad pública#
###############################################
#####
# 4 -- Grafico de nivel de pobreza segun la universidad publica a la que se asistio
#####
as.data.frame(prop.table(table(ENAHO$np, ENAHO$A15B), margin = 2)) %>% 
  mutate(Freq = round(Freq * 100, 2)) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  pivot_longer(cols = UCR:UTN,
               names_to = "Universidad",
               values_to = "Proporcion") %>% 
  mutate(Proporcion = Proporcion / 100) %>% 
  ggplot(aes(x = fct_relevel(Universidad, c("UNED", "UTN", "UNA", "UCR", "TEC")), 
             y = Proporcion, fill = Nivel_de_pobreza)) +
  geom_col(alpha = 0.85, color = "white", size = 0.3) +  # Mejora visual en las columnas
  labs(
    title = "Gráfico 4.\nNivel de pobreza según universidad pública a la que se asistió",
    x = "Universidad pública a la que se asistió",
    y = "Proporción acumulada",
    fill = "Nivel de pobreza",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),  # Mejora en el texto del eje X
    axis.text.y = element_text(size = 10, color = "gray20"),  # Mejora en el texto del eje Y
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico4.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 5 -- Grafico de la cantidad de personas pobres que fueron a una universidad publica, por universidad segun su formacion educativa formal
#####
ENAHO %>% 
  filter(!is.na(A15B) & !is.na(ForReg) & np != "No pobre" & A15B != "Ignorado") %>% 
  ggplot(aes(x = fct_infreq(A15B), fill = fct_infreq(ForReg))) +
  geom_bar(alpha = 0.85, color = "white", size = 0.3) +  # Ajuste visual en las barras
  labs(
    title = "Gráfico 5.\nCantidad de personas en condición de pobreza por universidad",
    subtitle = "según formación educativa formal",
    x = "Universidad pública a la que se asistió",
    y = "Cantidad de personas en condición de pobreza",
    fill = "Formación educativa formal",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_fill_brewer(palette = "Set2") +  # Paleta de colores mejorada
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),  # Ajuste en el texto del eje X
    axis.text.y = element_text(size = 10, color = "gray20"),  # Mejora en el texto del eje Y
    axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico5.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 6 -- Grafico de la proporcion de personas no pobres que fueron a universidad publica, por universidad segun formacion educativa formal
#####
ENAHO %>% 
  filter(!is.na(A15B) & !is.na(ForReg) & np == "No pobre" & A15B != "Ignorado" & ForReg != "Ignorada" & ForReg != "No definida") %>% 
  ggplot(aes(x = fct_infreq(A15B), fill = fct_infreq(ForReg))) +
  geom_bar(alpha = 0.85, color = "white", size = 0.3) +  # Ajuste en las barras
  labs(
    title = "Gráfico 6.\nCantidad de personas en condición de no pobreza por universidad",
    subtitle = "según formación educativa formal",
    x = "Universidad pública a la que se asistió",
    y = "Cantidad de personas en condición de no pobreza",
    fill = "Formación educativa formal",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_fill_brewer(palette = "Set2") +  # Paleta de colores mejorada
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    plot.caption = element_text(size = 6, hjust = 0),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 10)),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10, face = "bold", color = "gray20"),
    axis.text.x = element_text(size = 10, face = "bold", color = "gray20"),  # Ajuste del ángulo del texto en el eje X
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),  # Mejora en el título del eje Y
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),  # Mejora en el título del eje X
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico6.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 7 -- Graficos de distribucion del itpn segun universidad publica a la que se asistio
#####
#####
# 7.1 --
#####
ENAHO %>% 
  filter(A15B != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15B, itpn, median), fill = A15B)) +
  geom_density_ridges(alpha = 0.85, color = "white") +  # Ajuste en las curvas de densidad
  geom_vline(xintercept = 129038, color = "red", linetype = "dotted", size = 0.8) +  # Línea de pobreza mejorada
  annotate("text", y = 0.5, x = 180000, label = "Línea de pobreza", color = "red", 
           size = 3, hjust = 0, vjust = -0.5, fontface = "italic") +  # Texto mejorado
  labs(
    title = "Gráfico 7.1.\nDistribución del ingreso total neto por persona\nsegún universidad pública a la que se asistió",
    x = "Ingreso Personal Neto (₡, log10)",
    y = "Universidad pública a la que se asistió",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_x_log10(labels = scales::comma) +  # Escala logarítmica
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "none",  # Sin leyenda
    axis.text.y = element_text(size = 10, hjust = 1, color = "gray20", face = "bold"),  # Texto eje Y ajustado
    axis.text.x = element_text(size = 10, color = "gray20"),  # Texto eje X ajustado
    plot.caption = element_text(size = 6, hjust = 0),  # Fuente de datos
    plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),  # Ajuste del título
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico7.1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 7.2 --
#####
ENAHO %>% 
  filter(A15B != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15B, itpn, median), fill = A15B)) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, outlier.fill = "white", alpha = 0.8) +  # Se elimina la forma de los outliers para simplificar
  geom_vline(xintercept = 129038, color = "red", linetype = "dotted", size = 0.8) +  # Línea de pobreza
  annotate("text", y = 2.5, x = 25000, label = "Línea de pobreza", color = "red", 
           size = 3, hjust = 0, vjust = -0.5, fontface = "italic") +  # Texto mejorado
  labs(
    title = "Gráfico 7.2.\nDistribución del ingreso total neto por persona\nsegún universidad pública a la que se asistió",
    x = "Ingreso Personal Neto (₡, log10)",
    y = "Universidad pública a la que se asistió",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_x_log10(labels = scales::comma) +  # Escala logarítmica
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "none",  # Sin leyenda
    axis.text.y = element_text(size = 10, hjust = 1, color = "gray20", face = "bold"),  # Texto eje Y ajustado
    axis.text.x = element_text(size = 10, color = "gray20"),  # Texto eje X ajustado
    plot.caption = element_text(size = 6, hjust = 0),  # Ajuste de la fuente
    plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),  # Ajuste del título
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()  # Se eliminan las cuadrículas menores
  )
ggsave("../Manejo_de_Datos/graphs/Grafico7.2.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
################################################
#Gráficos relacionados con nivel de instruccion#
################################################
#####
# 8 -- Grafico de evolucion de nivel de pobreza segun nivel de instruccion
#####
as.data.frame(table(ENAHO$np, fct_collapse(ENAHO$NivInst, 
                                           "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                           "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                           "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta")))) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(NivInst = Var2) %>% 
  ggplot(aes(x = NivInst, y = Freq, fill = Nivel_de_pobreza)) + 
  geom_col(position = "fill", alpha = 0.8, color = "white", size = 0.3) +  # Ajustes visuales
  labs(
    title = "Gráfico 8.\nNivel de pobreza según nivel de instrucción",
    subtitle = "Relación entre nivel de instrucción y nivel de pobreza (Costa Rica, 2023)",
    x = "Nivel de instrucción",
    y = "Proporción acumulada",
    fill = "Nivel de pobreza",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_fill_viridis_d(option = "G") +  # Paleta de colores viridis
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",  # Posición de la leyenda
    legend.title = element_text(size = 10, color = "gray20", face = "bold"),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico8.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 9 -- Grafico de la evolucion de la estabilidad laboral segun nivel de instruccion
#####
as.data.frame(prop.table(table(ENAHO$Estabili, fct_collapse(ENAHO$NivInst, 
                                                            "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                                            "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                                            "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta"))), margin = 2)) %>% 
  rename(Estabilidad = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(NivInst = Var2) %>% 
  filter(Estabilidad == "Empleo permanente") %>% 
  ggplot(aes(x = NivInst, y = Freq, color = Estabilidad, group = Estabilidad)) +
  geom_line(color = "blue", size = 1) +  # Aumentar el grosor de la línea
  geom_point(color = "blue", size = 3) +  # Añadir puntos para mayor visibilidad
  labs(
    title = "Gráfico 9.\nPorcentaje de personas con empleo permanente por nivel de instrucción",
    x = "Nivel de instrucción",
    y = "Proporción acumulada",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "none",  # Ocultar la leyenda
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20", angle = -15, hjust = 0.5, vjust = 0, face = "bold"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico9.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 10 -- Grafico de porcentaje de personas satisfechas con su trabajo por nivel de instruccion
#####
as.data.frame(prop.table(table(ENAHO$InsLab, fct_collapse(ENAHO$NivInst, 
                                                          "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                                          "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                                          "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta"))), margin = 2)) %>% 
  rename(Satisfaccion = Var1) %>% 
  filter(Var2 != "Satisfacción ignorada") %>% 
  rename(NivInst = Var2) %>% 
  filter(Satisfaccion == "Satisfecho", NivInst != "Ignorado") %>% 
  ggplot(aes(x = NivInst, y = Freq, color = Satisfaccion, group = Satisfaccion)) +
  geom_line(size = 1.2) +  # Aumentar el grosor de la línea
  geom_point(size = 3) +  # Añadir puntos para mayor visibilidad
  labs(
    title = "Gráfico 10.\nPorcentaje de personas satisfechas con su empleo por nivel de instrucción",
    x = "Nivel de instrucción",
    y = "Proporción acumulada",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "none",  # Ocultar la leyenda
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20", angle = -15, hjust = 0.5, vjust = 0, face = "bold"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico10.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 11 -- Grafico: distribucion del itpn por nivel de instruccion (version ultra mejorada)
#####
ENAHO %>% 
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
    title = "Grafico 11.\nDistribución del ingreso total neto por persona \nsegún nivel de instrucción",
    subtitle = "Costa Rica, 2023",
    x = "Ingreso Personal Neto (₡, log10)",
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
    plot.title = element_text(size = 18, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico11.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 12 -- Grafico evolucion de la distribucion del nivel de instruccion por quintil de ingreso 
#####
as.data.frame(prop.table(table(fct_collapse(ENAHO$NivInst, 
                                            "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                            "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                            "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta")), 
                               ENAHO$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_area(alpha = 0.7, color = "white") +  # Agregar color de borde y ajustar opacidad
  labs(
    title = "Gráfico 12.\nDistribución del nivel de instrucción por quintil de ingreso per cápita Costa Rica, 2023",
    subtitle = "Relación entre el nivel de instrucción y nivel de ingreso per cápita según quintiles de ingresos",
    x = "Quintil de ingreso per cápita",
    y = "Proporción acumulada",
    fill = "Nivel de instrucción",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_fill_viridis_d(option = "G") +  # Paleta de colores viridis
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico12.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#######################################
#Gráficos relacionados con escolaridad#
#######################################
#####
# 13 -- Grafico correlacion entre años de escolaridad e ingreso total por persona neto
#####
ENAHO %>% 
  ggplot(aes(x = Escolari, y = itpn, color = ZONA)) +
  # Cambiar a puntos con transparencia para evitar solapamientos y tamaño adecuado
  geom_point(alpha = 0.6, size = 2) +
  # Línea de regresión más destacada, con mayor grosor
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  # Facetas según la zona
  facet_wrap(~ZONA) +
  # Etiquetas y títulos mejorados
  labs(title = "Gráfico 13. \nRelación entre años de escolaridad e ingreso personal neto",
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
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
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
ggsave("../Manejo_de_Datos/graphs/Grafico13.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 14 -- Grafico correlacion entre años de escolaridad e intensidad de la pobreza multidimensional
#####
ENAHO %>% 
  ggplot(aes(x = Escolari, y = IPM_Intensidad, color = ZONA)) +
  # Cambiar a puntos con transparencia para evitar solapamientos y tamaño adecuado
  geom_point(alpha = 0.6, size = 2) +
  # Línea de regresión más destacada, con mayor grosor
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  # Facetas según la zona
  facet_wrap(~ZONA) +
  # Etiquetas y títulos mejorados
  labs(title = "Gráfico 14. \nRelación entre años de escolaridad y la intensidad de la pobreza",
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
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
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
ggsave("../Manejo_de_Datos/graphs/Grafico14.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 15 -- 
#####
ENAHO %>% filter(!is.na(Q_IPCN)) %>% 
  ggplot(aes(x = Escolari, 
             y = itpn, 
             color = Q_IPCN)) +
  geom_jitter(alpha = 0.6, size = 1.5, width = 0.2) +  # Ajustar tamaño y opacidad
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Q_IPCN) +
  labs(
    title = "Gráfico 15.\nRelación entre los años de escolaridad e ingreso total por persona neto",
    x = "Años de escolaridad",
    y = "Ingreso Personal Neto (₡, log10)",
    colour = "Quintiles de ingreso per cápita \ndel hogar neto \npor región de planificación",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_y_log10(labels = scales::label_number()) +
  scale_colour_brewer(palette = "Set1") +  # Paleta de colores para los quintiles
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico15.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
###########################################
#Gráficos relacionados con título obtenido#
###########################################
#####
# 16 -- Grafico de evolucion de nivel de pobreza segun titulo
#####
as.data.frame(table(ENAHO$np, ENAHO$A16B)) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(titulo = Var2) %>% 
  ggplot(aes(x = titulo, y = Freq, fill = Nivel_de_pobreza)) + 
  geom_col(position = "fill", alpha = 0.8, color = "white", size = 0.3) +  # Transparencia y bordes
  labs(title = "Gráfico 16.\nNivel de pobreza según título obtenido",
       subtitle = "en porcentajes",
       x = "Título",
       y = NULL,
       fill = "Nivel de pobreza",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  theme_minimal(base_size = 14) +  # Estilo minimalista
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico16.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 17 -- Grafico de la evolucion de la estabilidad laboral segun titulo obtenido
#####
as.data.frame(prop.table(table(ENAHO$Estabili, ENAHO$A16B), margin = 2)) %>% 
  rename(Estabilidad = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(titulo = Var2) %>% 
  filter(Estabilidad == "Empleo permanente") %>% 
  ggplot(aes(x = titulo, y = Freq, color = Estabilidad, group = Estabilidad)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "blue", shape = 18) +
  labs(title = "Gráfico 17.\nPorcentaje de personas con empleo permanente según título obtenido",
       x = "Título",
       y = "Proporción acumulada",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 0, size = 8, angle = -20, face = "bold", color = "gray20"))
ggsave("../Manejo_de_Datos/graphs/Grafico17.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 18 -- Grafico de porcentaje de personas satisfechas con su trabajo por el titulo obtenido
#####
as.data.frame(prop.table(table(ENAHO$InsLab, ENAHO$A16B), margin = 2)) %>% 
  rename(Satisfaccion = Var1) %>% 
  filter(Var2 != "Satisfacción ignorada") %>% 
  rename(titulo = Var2) %>% 
  filter(Satisfaccion == "Satisfecho", titulo != "Ignorado") %>% 
  ggplot(aes(x = titulo, y = Freq, color = Satisfaccion, group = Satisfaccion)) +
  geom_line(color = "red") +
  geom_point(size = 3, color = "red2", shape = 18) +
  labs(title = "Gráfico 18.\nPorcentaje de personas satisfechas con su empleo según título obtenido",
       x = "Título",
       y = "Proporción acumulada",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(hjust = 0, size = 8, angle = -20, color = "gray20", face = "bold"))
ggsave("../Manejo_de_Datos/graphs/Grafico18.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 19.1 -- Grafico: distribucion del itpn por título obtenido (version ultra mejorada)
#####
ENAHO %>% 
  filter(A16B != "Ignorado" & itpn > 0) %>%
  ggplot(aes(x = itpn, y = fct_reorder(A16B, itpn, median), fill = A16B)) +
  # Mejorar la visualización con densidades y lineas más detalladas
  geom_density_ridges(rel_min_height = 0.01, scale = 1.2, alpha = 0.8, color = "white") + 
  # Etiquetas y títulos mejorados
  labs(
    title = "Grafico 19.1.\nDistribución del ingreso total neto por persona \nsegún título obtenido",
    subtitle = "Costa Rica, 2023",
    x = "Ingreso Personal Neto (₡, log10)",
    y = "Título obtenido",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  # Ajuste de escala con formato de coma en el eje x
  scale_x_log10(labels = scales::comma) +
  # Mejora de la paleta de colores con gradientes suaves
  scale_fill_viridis_d(option = "H") +
  # Temas mejorados con mayor atención a los detalles visuales
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Ocultar leyenda
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico19.1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 19.2 -- 
#####
ENAHO %>% 
  filter(A16B != "Ignorado" & itpn > 0) %>%
  ggplot(aes(x = itpn, y = fct_reorder(A16B, itpn, median), fill = A16B)) +
  # Boxplot con ajuste de grosor y transparencia
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, outlier.fill = "white", alpha = 0.8) +
  # Títulos mejorados con mayor tamaño y alineación
  labs(title = "Gráfico 19.2.\nDistribución del ingreso total neto por persona según título obtenido",
       subtitle = "Relación entre ingreso total neto y el tipo de título académico alcanzado",
       x = "Ingreso Personal Neto (₡, log10)",
       y = "Tipo de título académico",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  # Escala logarítmica en el eje X con separadores de miles
  scale_x_log10(labels = scales::comma) +
  # Mejora de la paleta de colores con gradientes suaves
  scale_fill_viridis_d(option = "H") +
  # Aplicación de un tema de cowplot con personalización
  theme_minimal(base_size = 14) +
  theme(
    # Título y subtítulo con mayor tamaño y centrado
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    # Ajustes de etiquetas de los ejes
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9, color = "gray20", face = "bold"),
    # Estilo del pie de página
    plot.caption = element_text(size = 7, hjust = 0),
    # Ocultar leyenda
    legend.position = "none",
    #ajustes panel
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
#####
# 20 -- Grafico evolucion de la distribucion de los títulos obtenidos por quintil de ingreso
#####
as.data.frame(prop.table(table(ENAHO$A16B, ENAHO$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = fct_reorder(Var1, Freq, median))) +
  geom_area(alpha = 0.8) +  # Transparencia para el área
  labs(title = "Gráfico 20. \nDistribución de títulos obtenidos por quintil de ingreso per cápita Costa Rica, 2023",
       x = "Quintil de ingreso per cápita",
       y = NULL,
       fill = "Título",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  # Mejora de la paleta de colores con gradientes suaves
  scale_fill_viridis_d(option = "H") +  
  theme_minimal(base_size = 14) +  # Estilo minimalista
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico20.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####################################################
#Gráficos relacionados con dominio de segundo idioma#
#####################################################
#####
# 21 -- 
#####
as.data.frame(table(ENAHO$np, ENAHO$A22A)) %>% 
  filter(Var2 != "Ignorado") %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_col(position = "fill", alpha = 0.8, color = "white", size = 0.3) +  # Gráfico de área
  labs(
    title = "Gráfico 21.\nNivel de pobreza según dominio de un segundo idioma",
    subtitle = "Relación entre el dominio de un segundo idioma y nivel de pobreza (Costa Rica, 2023)",
    x = "Dominio de un segundo idioma",
    y = "Proporción acumulada",
    fill = "Nivel de pobreza",
    caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales."
  ) +
  scale_fill_viridis_d(option = "D") +  # Paleta de colores viridis
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",  # Mover la leyenda arriba
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico21.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 22 -- Grafico evolucion de la distribucion de las personas que dominan un segundo idioma por quintil de ingreso 
#####
as.data.frame(prop.table(table(ENAHO$A22A, ENAHO$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_area(alpha = 0.8) +  # Añade transparencia para el área
  labs(title = "Gráfico 22. \nDistribución de personas que dominan un segundo idioma por quintil de ingreso per cápita Costa Rica, 2023",
       x = "Quintil de ingreso per cápita",
       y = NULL,
       fill = "Dominio de un segundo idioma",
       caption = "Fuente: Instituto Nacional de Estadística y Censos (INEC), Costa Rica. (2023). Encuesta Nacional de Hogares 2023, Julio 2023: Resultados Generales.") +
  scale_fill_brewer(palette = "Set1") +  # Cambia a una paleta más uniforme
  theme_minimal(base_size = 14) +  # Estilo minimalista
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico22.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
#####
# 23 -- Grafico: distribucion del itpn segun dominio de segundo idioma (version ultra mejorada)
#####
ENAHO %>% 
  filter(A22A != "Ignorado" & itpn > 0) %>%
  ggplot(aes(x = itpn, y = fct_reorder(A22A, itpn, median), fill = A22A)) +
  # Mejorar la visualización con densidades y lineas más detalladas
  geom_density_ridges(rel_min_height = 0.01, scale = 1.2, alpha = 0.8, color = "white") + 
  # Etiquetas y títulos mejorados
  labs(
    title = "Grafico 23.\nDistribución del ingreso total neto por persona según dominio de un segundo idioma",
    subtitle = "Costa Rica, 2023",
    x = "Ingreso Personal Neto (₡, log10)",
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
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )
ggsave("../Manejo_de_Datos/graphs/Grafico23.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 11.6, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada



