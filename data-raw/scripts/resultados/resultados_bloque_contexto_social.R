
# Bloque Contexto Social ----------------------------------------------------------------------

source(file = "./data-raw/scripts/parametros//parametros_bloque_contexto_social.R")

# pREGUNTA RANDOM
bd_temas <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(temas,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = temas)

g_temas <-
  bd_temas |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02) +
  scale_fill_manual(values = colores_temas) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_temas_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

# medios de comunicacion
bd_medios_com <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(medios_com,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = medios_com,
         pct = media)

g_medios_com <-
  bd_medios_com |>
  graficar_lollipops(width_cats = 35,size_pct = 5) +
  scale_color_manual(values = colores_medios_com) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_medios_com_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 12))

# Redes sociales que utiliza
bd_utiliza <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("utiliza"),pesos) |>
  tidyr::pivot_longer(cols = -pesos,#everything(),
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  group_by(pregunta) |>
  count(respuesta,wt = pesos) |>
  mutate(pct = n/sum(n, na.rm = TRUE)) |>
  ungroup() |>
  filter(respuesta == "Sí") |>
  left_join(diccionario |>
              distinct(llave, tema),
            by = c("pregunta" = "llave")) |>
  transmute(respuesta = tema,
            media = pct)

g_utiliza <-
  bd_utiliza |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.04) +
  scale_fill_manual(values = colores_utiliza) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = paste0(p_utiliza_tit,
                        "\n",
                        "El resultado corresponde a los que contestaron que Sí")) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

# Problemas de Chile
bd_problema_chile <-
  # bd_respuestas_efectivas |>
  # as_tibble() |>
  # select(SbjNum, contains("problema_chile_O"),pesos) |>
  # tidyr::pivot_longer(cols = !c(SbjNum,pesos),
  #                     names_to = "pregunta",
  #                     values_to = "respuesta") |>
  # na.omit() |>
  # count(respuesta) |>
  # mutate(pct = n/nrow(bd_respuestas_efectivas))
bd_respuestas_efectivas |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname, contains("problema_chile_O"),pesos) |>
  mutate(tot_pesos = sum(pesos)) |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion_aux = 1.0) %>%
  select(-name) %>%
  distinct(rowname, pesos, tot_pesos, value,seleccion_aux) |>
  tidyr::pivot_wider( id_cols = c(rowname, pesos,tot_pesos ),names_from = value, values_from = seleccion_aux,values_fill = 0)%>%
  select(-rowname) %>%
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-tot_pesos, names_to = "respuesta",values_to = "value") %>%
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max))

g_problema_chile <-
  bd_problema_chile |>
  graficar_lollipops(width_cats = 35,size_pct = 5) +
  scale_color_manual(values = colores_problema) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_problema_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))

# Calificaciones gobierno Boric

# Delincuencia
resultados_cali_delincuencia <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_delincuencia")

g_cali_delincuencia <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_delincuencia) |>
  ggplot(aes(x = as.numeric(cali_delincuencia))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_delincuencia$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_delincuencia$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_delincuencia$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0("",
                        #stringr::str_to_title(" delincuencia"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_delincuencia$pct_nsnc, accuracy = 1.)),
       title = 'Delincuencia') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


# bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(starts_with("cali_")) |>
#   glimpse()

resultados_cali_educacion <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_educacion")

g_cali_educacion <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_educacion) |>
  ggplot(aes(x = as.numeric(cali_educacion))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_educacion$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_educacion$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_educacion$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        #stringr::str_to_title(" educación"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_educacion$pct_nsnc, accuracy = 1.)),
       title = 'Educación') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

resultados_cali_salud <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_salud")

g_cali_salud <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_salud) |>
  ggplot(aes(x = as.numeric(cali_salud))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_salud$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_salud$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_salud$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0("",#p_calificacion_gobierno,
                        #stringr::str_to_title(" salud"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_salud$pct_nsnc, accuracy = 1.)),
       title = 'Salud') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

resultados_cali_empleo <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_empleo")

g_cali_empleo <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_empleo) |>
  ggplot(aes(x = as.numeric(cali_empleo))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_empleo$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_empleo$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_empleo$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        #stringr::str_to_title(" empleo"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_empleo$pct_nsnc, accuracy = 1.)),
       title = 'Empleo') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

resultados_cali_pensiones <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_pensiones")

g_cali_pensiones <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_pensiones) |>
  ggplot(aes(x = as.numeric(cali_pensiones))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_pensiones$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_pensiones$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_pensiones$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0("",#p_calificacion_gobierno,
                        #stringr::str_to_title(" pensiones"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_pensiones$pct_nsnc, accuracy = 1.)),
       title = 'Pensiones') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

resultados_cali_ambiente <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_ambiente")

g_cali_ambiente <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_ambiente) |>
  ggplot(aes(x = as.numeric(cali_ambiente))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_ambiente$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_ambiente$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_ambiente$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        #stringr::str_to_title(" ambiente"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_ambiente$pct_nsnc, accuracy = 1.)),
       title = 'Medio Ambiente') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

resultados_cali_inmigracion <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_inmigracion")

g_cali_inmigracion <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_inmigracion) |>
  ggplot(aes(x = as.numeric(cali_inmigracion))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_inmigracion$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_inmigracion$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_inmigracion$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0("",#p_calificacion_gobierno,
                        #stringr::str_to_title(" inmigración"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_inmigracion$pct_nsnc, accuracy = 1.)),
       title = 'Inmigración') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

resultados_cali_derechosmujer <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_derechosmujer")

g_cali_derechosmujer <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_derechosmujer) |>
  ggplot(aes(x = as.numeric(cali_derechosmujer))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_derechosmujer$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_derechosmujer$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_derechosmujer$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        #stringr::str_to_title(" inmigración"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_derechosmujer$pct_nsnc, accuracy = 1.)),
       title = 'Derechos de la Mujer') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

resultados_cali_economia <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_economia")

g_cali_economia <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_economia) |>
  ggplot(aes(x = as.numeric(cali_economia))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_economia$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_economia$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_economia$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        #stringr::str_to_title(" inmigración"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_economia$pct_nsnc, accuracy = 1.)),
       title = 'Economía') +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

# Calificacion gobierno
resultados_cali_desem_vec <- c("resultados_cali_delincuencia",
                               "resultados_cali_educacion",
                               "resultados_cali_salud",
                               "resultados_cali_empleo",
                               "resultados_cali_pensiones",
                               "resultados_cali_ambiente",
                               "resultados_cali_inmigracion",
                               "resultados_cali_derechosmujer",
                               "resultados_cali_economia")



bd_cali_desem<-
  resultados_cali_desem_vec |>
  purrr::map_df(.f= ~{
    as.data.frame(eval(rlang::sym(.x))) |>
      mutate(resultados = .x)
  })|>
  mutate(aspecto =  gsub("resultados_","",resultados) ) |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  mutate(inf = media,
         sup =  media
  )



p_cali_desem_graf <-
  bd_cali_desem |>
  graficar_intervalo_numerica(escala = c(1,7),text_point_size = 6,point_size = .7,nudge_x = .4) +
  geom_vline(xintercept = c(1:10), linetype = "dotted",alpha = .3) +
  labs(caption = p_calificacion_gobierno) +
  scale_y_continuous(breaks = c(1:7),limits = c(1,7))+
  tema_morant()


# Chile actual
bd_chile_actual <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(chile_actual,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = chile_actual)

g_chile_actual <-
  bd_chile_actual |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(c("Progresando",
                                           "Estancado",
                                           "En retroceso",
                                           "Ns/Nc"))) +
  scale_fill_manual(values = colores_chile_actual) +
  scale_y_continuous(limits = c(0, 0.6),
                     labels = scales::percent) +
  labs(caption = p_chile_actual_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

# Chile futuro
bd_chile_futuro <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(chile_futuro,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = chile_futuro)

g_chile_futuro <-
  bd_chile_futuro |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(c("Mejor que hoy",
                                           "Igual que hoy",
                                           "Peor que hoy",
                                           "Ns/Nc"))) +
  scale_fill_manual(values = colores_chile_futuro) +
  scale_y_continuous(limits = c(0, 0.6),
                     labels = scales::percent) +
  labs(caption = p_chile_futuro_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

# Frases ricos
bd_frases_ricos <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(frases_ricos,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = frases_ricos)

g_frases_ricos <-
  bd_frases_ricos |>
  graficar_barras(salto = 25,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02) +
  scale_fill_manual(values = colores_frases_ricos) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_frases_ricos_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 12))

# Frases gobierno
bd_frases_gobierno <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(frases_gobierno,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = frases_gobierno)

g_frases_gobierno <-
  bd_frases_gobierno |>
  graficar_barras(salto = 25,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02) +
  scale_fill_manual(values = colores_frases_gobierno) +
  scale_y_continuous(limits = c(0, .75),
                     labels = scales::percent) +
  labs(caption = p_frases_gobierno_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 12))

# Satisfaccion democracia
bd_satisfaccion_democracia <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(satisfaccion_democracia,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = satisfaccion_democracia)

g_satisfaccion_democracia <-
  bd_satisfaccion_democracia |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.04,
                  orden_respuestas = rev(c("Muy satisfecho",
                                           "Algo satisfecho",
                                           "Ni satisfecho ni insatisfecho",
                                           "Algo insatisfecho",
                                           "Muy insatisfecho",
                                           "Ns/Nc"))) +
  scale_fill_manual(values = colores_satisfaccion_democracia) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_satisfaccion_democracia_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

# Izquierda vs derecha

variables_izq_der <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(starts_with("escala")) |>
  names()

# bd_variables_izq_der <-
#   variables_izq_der %>%
#   purrr::map_df(.x = .,
#                 .f = ~ tibble("variable" = .x,
#                               "media" = calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
#                                                                          variable = .x) %>%
#                                 purrr::pluck("media"))) |>
#   mutate(izq = case_when(variable == "escala_bienestar" ~ "bienestar_izq",
#                          variable == "escala_ayuda" ~ "ayuda_izq",
#                          variable == "escala_economia" ~ "economia_izq",
#                          variable == "escala_aborto" ~ "aborto_izq",
#                          variable == "escala_gays" ~ "gays_izq"),
#          der = case_when(variable == "escala_bienestar" ~ "bienestar_der",
#                          variable == "escala_ayuda" ~ "ayuda_der",
#                          variable == "escala_economia" ~ "economia_der",
#                          variable == "escala_aborto" ~ "aborto_der",
#                          variable == "escala_gays" ~ "gays_der"))

bd_variables_izq_der <-
  variables_izq_der %>%
  purrr::map_df(.x = .,
                .f = ~ tibble("variable" = .x,
                              "media" = calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,limtes = c(1,5),
                                                                         max_min = F,
                                                                         variable = .x) %>%
                                purrr::pluck("media"))) |>
  mutate(izq = case_when(variable == "escala_bienestar" ~ "El Estado es el responsable del bienestar de las personas",
                         variable == "escala_ayuda" ~ "Todos los ciudadanos deben recibir la misma ayuda del Estado",
                         variable == "escala_economia" ~ "Es preferible que el funcionamiento de la economía esté basado en la planificación del Estado",
                         variable == "escala_aborto" ~ "El aborto debe ser permitido sin restricciones legales",
                         variable == "escala_gays" ~ "Las parejas del mismo sexo deben tener el derecho a casarse legalmente"),
         ###derecha
         der = case_when(variable == "escala_bienestar" ~ "Cada persona es responsable de su propio bienestar",
                         variable == "escala_ayuda" ~ "La ayuda del Estado debe destinarse solo a los más pobres",
                         variable == "escala_economia" ~ "Es preferible que el funcionamiento de la economía esté basado en la operación del libre mercado",
                         variable == "escala_aborto" ~ "El aborto no debería permitirse bajo ninguna circunstancia",
                         variable == "escala_gays" ~ "El matrimonio solo debe de darse entre un hombre y una mujer"))%>%
  mutate(y_numeric = as.numeric(forcats::fct_reorder(izq,media )))

escala_izq_der_graf<-
bd_variables_izq_der |>
  ggplot(aes(y = y_numeric,
             x = media)) +
  geom_point(size = 4,colour = "#850D2D") +
  geom_vline(xintercept = 3,linetype = 'dashed',colour = 'red',linewidth = 1,alpha = .2) +
  geom_text(aes(label = round(media, digits = 1)),
            nudge_y = .19,
            #vjust = -.7,
            size = 6) +
  scale_y_continuous(breaks = bd_variables_izq_der$y_numeric,
                     labels = bd_variables_izq_der$izq |> stringr::str_wrap(width = 40),
                     sec.axis = sec_axis(~.,
                                         breaks = bd_variables_izq_der$y_numeric,
                                         labels = bd_variables_izq_der$der |> stringr::str_wrap(width = 40)   ))+
  # coord_flip() +
  scale_x_continuous(limits = c(1, 5.5),
                     breaks = 1:5) +
  labs(caption = p_izquierda_derecha_tit)+
  tema_morant() +
  theme(axis.text.y  = element_text(size = 12),
        plot.caption = element_text(size = 12,hjust = 1),
        plot.caption.position = "plot")



# Interes Politica
bd_interes_politica<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(interes_politica,pesos) |>
  count(interes_politica,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=interes_politica )


p_interes_politica_graf<-
  bd_interes_politica|>
  graficar_barras(orden_respuestas = rev(orden_interes_politica),salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02)+
  scale_fill_manual(values=colores_interes_politica)+
  labs(caption = p_interes_politica_tit) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))
