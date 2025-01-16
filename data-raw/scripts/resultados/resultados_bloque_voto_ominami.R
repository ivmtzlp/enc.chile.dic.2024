##### Voto MEO
############################################


# Chile actual
bd_chile_actual_ominami <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  filter(voto_pr == "Marco Enríquez-Ominami") |>
  count(chile_actual,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = chile_actual)

g_chile_actual_ominami <-
  bd_chile_actual_ominami |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(c("Progresando",
                                           "Estancado",
                                           "En retroceso",
                                           "Ns/Nc"))) +
  scale_fill_manual(values = colores_chile_actual) +
  scale_y_continuous(limits = c(0, .75),
                     labels = scales::percent) +
  labs(caption = p_chile_actual_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

# Chile futuro
bd_chile_futuro_ominami <-
  bd_respuestas_efectivas |>
  filter(voto_pr == "Marco Enríquez-Ominami") |>
  as_tibble() |>
  count(chile_futuro,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = chile_futuro)

g_chile_futuro_ominami <-
  bd_chile_futuro_ominami |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(c("Mejor que hoy",
                                           "Igual que hoy",
                                           "Peor que hoy",
                                           "Ns/Nc"))) +
  scale_fill_manual(values = colores_chile_futuro) +
  scale_y_continuous(limits = c(0, 0.75),
                     labels = scales::percent) +
  labs(caption = p_chile_futuro_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



#########################################33
# Izquierda vs derecha
#############################################

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

bd_variables_izq_der_ominami <-
  variables_izq_der %>%
  purrr::map_df(.x = .,
                .f = ~ tibble("variable" = .x,
                              "media" = calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas |>
                                                                           filter(voto_pr == "Marco Enríquez-Ominami"),
                                                                         limtes = c(1,5),
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

escala_izq_der_ominami_graf<-
  bd_variables_izq_der_ominami |>
  ggplot(aes(y = y_numeric,
             x = media)) +
  geom_point(size = 4,colour = "#850D2D") +
  geom_vline(xintercept = 3,linetype = 'dashed',colour = 'red',linewidth = 1,alpha = .2) +
  geom_text(aes(label = round(media, digits = 1)),
            nudge_y = .19,
            #vjust = -.7,
            size = 6) +
  scale_y_continuous(breaks = bd_variables_izq_der_ominami$y_numeric,
                     labels = bd_variables_izq_der_ominami$izq |> stringr::str_wrap(width = 40),
                     sec.axis = sec_axis(~.,
                                         breaks = bd_variables_izq_der_ominami$y_numeric,
                                         labels = bd_variables_izq_der_ominami$der |> stringr::str_wrap(width = 40)   ))+
  # coord_flip() +
  scale_x_continuous(limits = c(1, 5.5),
                     breaks = 1:5) +
  labs(caption = p_izquierda_derecha_tit)+
  tema_morant() +
  theme(axis.text.y  = element_text(size = 12),
        plot.caption = element_text(size = 12,hjust = 1),
        plot.caption.position = "plot")


#################################3

#########################################33
# postura idelogica
#############################################

# postura ideologica
bd_definicion_postura_ideologica_ominami<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  filter(voto_pr == "Marco Enríquez-Ominami") |>
  select(definicion_postura_ideologica,pesos) |>
  #filter(!is.na(definicion_postura_ideologica)) |>
  count(definicion_postura_ideologica,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=definicion_postura_ideologica)



p_definicion_postura_ideologica_ominami_graf<-
  bd_definicion_postura_ideologica_ominami|>
  #graficar_barras()+
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(orden_definicion_postura_ideologica))+
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  scale_fill_manual(values = colores_definicion_postura_ideologica) +
  labs(caption = p_definicion_postura_ideologica_tit)+
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



#########################################33
# Ingreso
#############################################

bd_ingreso_mensual_hogar_ominami <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  filter(voto_pr == "Marco Enríquez-Ominami") |>
  count(ingreso_mensual_hogar,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = ingreso_mensual_hogar)

g_ingreso_mensual_hogar_ominami <-
  bd_ingreso_mensual_hogar_ominami |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = orden_ingreso_mensual_hogar) +
  scale_fill_manual(values = colores_ingreso_mensual_hogar) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_ingreso_mensual_hogar_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))





###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################


add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Perfil de quienes votarían por \n  Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
  ph_with(value = g_chile_actual_ominami, location = ph_location_label(ph_label = "grafica_uno")) |>
  ph_with(value = g_chile_futuro_ominami, location = ph_location_label(ph_label = "grafica_dos")) |>
  ph_with(value = 'Perspectiva de mejora en la vida de los chilenos',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = escala_izq_der_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Posicionamiento político de los chilenos frente a temas sociales',
          location = ph_location_label(ph_label = "titulo"))



add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_definicion_postura_ideologica_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Postura ideológica de los entrevistados',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_ingreso_mensual_hogar_ominami, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Ingreso mensual del hogar',
          location = ph_location_label(ph_label = "titulo"))



