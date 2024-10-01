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

