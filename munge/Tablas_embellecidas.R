#####################################################
#Tablas relacionadas con el tipo de centro educativo#
#####################################################

#Tabla: nivel de pobreza segun tipo de centro educativo al que se asistio (porcentajes)

tab_np_vs_tipo_colegio <- as.data.frame(prop.table(table(ENAHO$np, ENAHO$A15A), margin = 2)) %>% 
  mutate(Freq = round(Freq * 100,2)) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% pivot_wider(names_from = Var2, values_from = Freq)

kable(tab_np_vs_tipo_colegio,
      caption = "Hola")

kable(tabla3, 
      caption = "Resumen del ingreso total por persona neto según nivel de instrucción", 
      col.names = c("Nivel de instrucción", "Media", "Promedio", "Desviación estándar")) %>%
  kable_styling(position = "center", 
                bootstrap_options = c("striped", "hover", "responsive")) %>%
  footnote(general = "INEC, Costa Rica. (2023). ENAHO 2023, Julio 2023: Resultados Generales.",
           footnote_as_chunk = TRUE,
           general_title = "Fuente: ")