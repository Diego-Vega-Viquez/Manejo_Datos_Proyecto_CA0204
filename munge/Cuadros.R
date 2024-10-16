#Cuadros primaria, secundaria o estudios postsecundarios y pobreza
cuadro_primaria_completa <- datos_jc %>% 
  filter(!is.na(np)) %>%
  group_by(Tiene_prim_completa, np) %>% 
  summarise(conteo = n(), 
            .groups = 'drop') %>%
  group_by(Tiene_prim_completa) %>%  # Agrupar por Tiene_prim_completa para calcular el porcentaje correcto
  mutate(porcentaje = (conteo / sum(conteo)) * 100)  # Actualizar el porcentaje

cuadro_secundaria_completa <- datos_jc %>% 
  filter(!is.na(np)) %>%
  group_by(Tiene_sec_completa, np) %>% 
  summarise(conteo = n(), 
            .groups = 'drop') %>%
  group_by(Tiene_sec_completa) %>%  # Agrupar por Tiene_prim_completa para calcular el porcentaje correcto
  mutate(porcentaje = (conteo / sum(conteo)) * 100)  # Actualizar el porcentaje

cuadro_estudios_postsecundarios <- datos_jc %>% 
  filter(!is.na(np)) %>%
  group_by(Tiene_est_postsec, np) %>% 
  summarise(conteo = n(), 
            .groups = 'drop') %>%
  group_by(Tiene_est_postsec) %>%  # Agrupar por Tiene_prim_completa para calcular el porcentaje correcto
  mutate(porcentaje = (conteo / sum(conteo)) * 100)  # Actualizar el porcentaje


#Cuadro pobreza segun nivel de instrucción

datos_jc %>%
  filter(!is.na(np) & NivInst != "Ignorado") %>% 
  group_by(NivInst, np) %>% 
  summarise(conteo = n()) %>% #view()
  pivot_wider(names_from = np, values_from = conteo, values_fill = 0) %>% view()

#Cuadro porcentaje de pobreza multidimensional segun nivel de instrucción

datos_jc %>%
  filter(!is.na(IPM_Pobreza) & NivInst != "Ignorado") %>% 
  group_by(NivInst, IPM_Pobreza) %>% 
  summarise(conteo = n(), .groups = 'drop') %>%  # Contar el número de personas por grupo
  group_by(NivInst) %>%  # Agrupar nuevamente por nivel de instrucción
  mutate(porcentaje = conteo / sum(conteo) * 100) %>%  # Calcular el porcentaje
  select(NivInst, IPM_Pobreza, porcentaje) %>%  # Seleccionar columnas relevantes
  pivot_wider(names_from = IPM_Pobreza, values_from = porcentaje) %>% view()

#Cuadro ingreso segun nivel de instruccion: media y desviación estándar y cuartiles

datos_jc %>% 
  filter(itpn > 0 & NivInst != "Ignorado") %>% 
  group_by(NivInst) %>% 
  summarise(min = min(itpn), Q1 = quantile(itpn, 0.25), med = median(itpn), Q3 = quantile(itpn, 0.75), max = max(itpn), ingreso_promedio = mean(itpn), desv_est = sd(itpn)) %>% view()

#cuadro pobreza e ingreso segun universidad publica a la que asistió

datos_jc %>% 
  filter(!is.na(A15B) & A15B != "Ignorado" & !is.na(np)) %>% 
  group_by(A15B, np) %>% 
  summarise(conteo = n(), .groups = 'drop') %>% 
  group_by(A15B) %>% 
  mutate(porcentaje = conteo / sum(conteo) * 100) %>% 
  select(A15B, np, porcentaje) %>% 
  pivot_wider(names_from = np, values_from = porcentaje) %>% 
  mutate(Pobre = `Pobreza extrema` + `Pobreza no extrema`) %>% 
  select(A15B, Pobre)
  view()

  ####################################################################
  
  #####################################################
  #Tablas relacionadas con el tipo de centro educativo#
  #####################################################
  
  #Tabla: nivel de pobreza segun tipo de centro educativo al que se asistio (porcentajes)

tab_np_vs_tipo_colegio <- as.data.frame(prop.table(table(datos_jc$np, datos_jc$A15A), margin = 2))
tab_np_vs_tipo_colegio <- tab_np_vs_tipo_colegio %>% mutate(Freq = round(Freq * 100,2)) %>% rename(Nivel_de_pobreza = Var1) %>% filter(Var2 != "Ignorado")
tab_np_vs_tipo_colegio <- tab_np_vs_tipo_colegio %>% pivot_wider(names_from = Var2, values_from = Freq)
view(tab_np_vs_tipo_colegio)

#Tabla: distribucion del ingreso segun tipo de centro educativo al que se asistió

tab_dist_itpn_tipo_colegio <- datos_jc %>% 
  filter(A15A != "Ignorado") %>% 
  group_by(A15A) %>%
  summarize(
    n = n(),                      # Conteo de observaciones
    media_itpn = mean(itpn, na.rm = TRUE),   # Media de itpn, omitiendo NA
    ds_itpn = sd(itpn, na.rm = TRUE),       # Desviación estándar de itpn
    min_itpn = min(itpn, na.rm = TRUE),     # Mínimo de itpn
    Q1_itpn = quantile(itpn, 0.25, na.rm = TRUE),  # Primer cuartil
    Q2_itpn = quantile(itpn, 0.50, na.rm = TRUE),  # Mediana (segundo cuartil)
    Q3_itpn = quantile(itpn, 0.75, na.rm = TRUE),    
    max_itpn = max(itpn, na.rm = TRUE)      # Máximo de itpn
  )
view(tab_dist_itpn_tipo_colegio)

#Tabla de evolucion de la distribucion del tipo de centro educativo por quintil de ingreso per capita neto
tab_evol_tipo_colegio_QIPCN <- as.data.frame(prop.table(table(datos_jc$A15A, datos_jc$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
view(tab_evol_tipo_colegio_QIPCN)

#############################################
#Tablas relacionadas con universidad pública#
#############################################

#Tabla: nivel de pobreza segun universidad pública a la que se asistió (porcentajes)

tab_np_vs_univ <- as.data.frame(prop.table(table(datos_jc$np, datos_jc$A15B), margin = 2)) %>% 
  mutate(Freq = round(Freq * 100, 2)) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
view(tab_np_vs_univ)

#Tabla: distribucion del ingreso segun tipo de centro educativo al que se asistió

tab_dist_itpn_univ <- datos_jc %>% 
  filter(A15B != "Ignorado") %>% 
  mutate(A15B = fct_reorder(A15B, itpn, median, .desc = TRUE)) %>% 
  group_by(A15B) %>%
  summarize(
    n = n(),                      # Conteo de observaciones
    media_itpn = mean(itpn, na.rm = TRUE),   # Media de itpn, omitiendo NA
    ds_itpn = sd(itpn, na.rm = TRUE),       # Desviación estándar de itpn
    min_itpn = min(itpn, na.rm = TRUE),     # Mínimo de itpn
    Q1_itpn = quantile(itpn, 0.25, na.rm = TRUE),  # Primer cuartil
    Q2_itpn = quantile(itpn, 0.50, na.rm = TRUE),  # Mediana (segundo cuartil)
    Q3_itpn = quantile(itpn, 0.75, na.rm = TRUE),    
    max_itpn = max(itpn, na.rm = TRUE)      # Máximo de itpn
  )
view(tab_dist_itpn_univ)
 
##############################################
#Tablas relacionadas con nivel de instrucción#
##############################################

#Tabla: nivel de pobreza segun nivel de instruccion 
tab_np_vs_nivinst <- as.data.frame(prop.table(table(datos_jc$np, datos_jc$NivInst), margin = 2)) %>% 
  mutate(Freq = round(Freq * 100, 2)) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
view(tab_np_vs_nivinst)

#Tabla: estabilidad laboral segun nivel de instruccion 
tab_estabili_vs_nivinst <- as.data.frame(prop.table(table(datos_jc$Estabili, datos_jc$NivInst), margin = 2)) %>% 
  mutate(Freq = round(Freq * 100, 2)) %>% 
  rename(Estabilidad_laboral = Var1) %>% 
  filter(Var2 != "Ignorado", Estabilidad_laboral != "Estabilidad Ignorada") %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
view(tab_estabili_vs_nivinst)

#Tabla: satisfaccion laboral segun nivel de instruccion 
tab_satisf_vs_nivinst <- as.data.frame(prop.table(table(datos_jc$InsLab, datos_jc$NivInst), margin = 2)) %>% 
  mutate(Freq = round(Freq * 100, 2)) %>% 
  rename(Satisfaccion_laboral = Var1) %>% 
  filter(Var2 != "Ignorado", Satisfaccion_laboral != "Satisfacción ignorada") %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
view(tab_satisf_vs_nivinst)




