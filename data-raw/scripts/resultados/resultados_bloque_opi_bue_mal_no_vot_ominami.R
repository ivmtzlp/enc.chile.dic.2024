#################################################
#################################################
#################################################
# Opinion buena mala
#################################################
#################################################
#################################################

bd_op_ominami <-
bd_respuestas_efectivas |>
  filter(conoce_per_ominami == "Sí") |>
  count(opinion_ominami,wt = pesos) |>
  mutate(coef_op = n/sum(n),
         coef_op =  scales::percent(coef_op,accuracy =1.0)) |>
  select(-n)



sexo_gen_op_ominami_graf<-
#base
bd_respuestas_efectivas |>
  filter(conoce_per_ominami == "Sí") |>
  filter(!sexo == "-") |>
  filter(opinion_ominami != "Ns/Nc (No leer)") |>
  #mutate(opinion_ominami = ifelse(opinion_ominami == "Ns/Nc (No leer)","Ns/Nc",opinion_ominami )) |>
  count(opinion_ominami,sexo,generacion,wt = pesos) |>
  group_by(opinion_ominami) |>
  mutate(coef = n/sum(n)) |>
  ungroup() |>
  mutate(coef = case_when(sexo == "Hombre" ~ -coef,
                          T  ~ coef)) %>%
  left_join(bd_op_ominami,by = "opinion_ominami") |>
  mutate(opinion_ominami = paste0(opinion_ominami," (",coef_op,")")) |>
  #grafica
  ggplot(aes(x = generacion, y = coef, fill = sexo, label = scales::percent(abs(coef), 1))) +
  ggchicklet::geom_chicklet(alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c(color_h, color_m)) +
  ggfittext::geom_bar_text(contrast = TRUE) +
  lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
  tema_morant() +
  #division
  facet_wrap(~opinion_ominami)+
  theme(legend.title = element_blank(),
        axis.title.y = element_text(family = font_family, colour = font_color, size = 16),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        plot.background = element_rect(color = "transparent",fill = "transparent"),
        panel.background = element_rect(color = "transparent",fill = "transparent")) +
  labs(x = "Rango de edad",y=NULL,
       caption = 'Rango de edad y sexo de las personas \n que tienen una opinón (...) de Ominami')




# Satisfaccion democracia
bd_satisfaccion_democracia_op_ominami <-
  bd_respuestas_efectivas |>
  filter(conoce_per_ominami == "Sí") |>
  filter(opinion_ominami != "Ns/Nc (No leer)") |>
  #mutate(opinion_ominami = ifelse(opinion_ominami == "Ns/Nc (No leer)","Ns/Nc",opinion_ominami )) |>
  as_tibble() |>
  count(opinion_ominami,satisfaccion_democracia,wt = pesos) |>
  na.omit() |>
  group_by(opinion_ominami) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = satisfaccion_democracia) %>%
  left_join(bd_op_ominami,by = "opinion_ominami") |>
  mutate(opinion_ominami = paste0(opinion_ominami," (",coef_op,")"))

g_satisfaccion_democracia_op_ominami <-
  bd_satisfaccion_democracia_op_ominami |>
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
  scale_y_continuous(limits = c(0, .5),
                     labels = scales::percent) +
  labs(caption = paste0(p_satisfaccion_democracia_tit,
                        "\n Por quienes tienen una opinón (...) de Ominami ")) +
  tema_morant() +
  facet_wrap(~opinion_ominami)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))


# Voto Pr 25
bd_voto_pr_op_ominami<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  filter(conoce_per_ominami == "Sí") |>
  filter(opinion_ominami != "Ns/Nc (No leer)") |>
  #mutate(opinion_ominami = ifelse(opinion_ominami == "Ns/Nc (No leer)","Ns/Nc",opinion_ominami )) |>
  select(opinion_ominami,voto_pr,pesos) |>
  filter(!is.na(voto_pr)) |>
  group_by(opinion_ominami) |>
  count(opinion_ominami,voto_pr,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_pr) %>%
  left_join(bd_op_ominami,by = "opinion_ominami") |>
  mutate(opinion_ominami = paste0(opinion_ominami," (",coef_op,")"))


p_voto_pr_op_ominami_graf<-
  bd_voto_pr_op_ominami|>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.05)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_voto_pr_tit)+
  tema_morant()+
  facet_wrap(~opinion_ominami)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


# Voto segundo Pr 25
bd_voto2_pr_op_ominami<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  filter(conoce_per_ominami == "Sí") |>
  filter(opinion_ominami != "Ns/Nc (No leer)") |>
  #mutate(opinion_ominami = ifelse(opinion_ominami == "Ns/Nc (No leer)","Ns/Nc",opinion_ominami )) |>
  select(opinion_ominami,voto2_pr,pesos) |>
  filter(!is.na(voto2_pr)) |>
  group_by(opinion_ominami) |>
  count(opinion_ominami,voto2_pr,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto2_pr) %>%
  left_join(bd_op_ominami,by = "opinion_ominami") |>
  mutate(opinion_ominami = paste0(opinion_ominami," (",coef_op,")"))


p_voto2_pr_op_ominami_graf<-
  bd_voto2_pr_op_ominami|>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.05)+
  #graficar_barras(orden_respuestas = rev(orden_voto2_pr))+
  labs(caption = p_voto2_pr_tit)+
  scale_fill_manual(values = colores_voto2_pr ) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant()+
  facet_wrap(~opinion_ominami)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



# candidato nunca voto pr 25
bd_candidato_nunca_voto_op_ominami<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  filter(conoce_per_ominami == "Sí") |>
  filter(opinion_ominami != "Ns/Nc (No leer)") |>
  #mutate(opinion_ominami = ifelse(opinion_ominami == "Ns/Nc (No leer)","Ns/Nc",opinion_ominami )) |>
  select(opinion_ominami,candidato_nunca_voto,pesos) |>
  filter(!is.na(candidato_nunca_voto)) |>
  group_by(opinion_ominami) |>
  count(opinion_ominami,candidato_nunca_voto,wt=pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=candidato_nunca_voto)%>%
  left_join(bd_op_ominami,by = "opinion_ominami") |>
  mutate(opinion_ominami = paste0(opinion_ominami," (",coef_op,")"))


p_candidato_nunca_voto_op_ominami_graf<-
  bd_candidato_nunca_voto_op_ominami|>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.02)+
  #graficar_barras(orden_respuestas = rev(orden_candidato_nunca_voto))+
  labs(caption = p_candidato_nunca_voto_tit)+
  scale_fill_manual(values = colores_candidato_nunca_voto ) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant()+
  facet_wrap(~opinion_ominami)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



# Cualidad mas valorada

bd_cualidades_valora_candidato_op_ominami <-
  bd_respuestas_efectivas |>
  filter(conoce_per_ominami == "Sí") |>
  filter(opinion_ominami != "Ns/Nc (No leer)") |>
  #mutate(opinion_ominami = ifelse(opinion_ominami == "Ns/Nc (No leer)","Ns/Nc",opinion_ominami )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,opinion_ominami ,contains("cualidades_valora_candidato_O"),pesos) |>
  group_by(opinion_ominami) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,opinion_ominami,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(opinion_ominami) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,opinion_ominami), names_to = "respuesta",values_to = "value") %>%
  group_by(opinion_ominami) |>
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max)) |>
    left_join(bd_op_ominami,by = "opinion_ominami") |>
    mutate(opinion_ominami = paste0(opinion_ominami," (",coef_op,")"))


p_cualidades_valora_candidato_op_ominami_graf <-
  bd_cualidades_valora_candidato_op_ominami |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_cualidades_valora_candidato) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  tema_morant() +
  labs(caption = "¿Cuál de las siguientes cualidades valora más en un \ncandidato presidencial? Me puede\ndecir tres cualidades, por favor") +
  facet_wrap(~opinion_ominami)+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))

#################################################
#################################################
#################################################
# Nunaca Voto
#################################################
#################################################
#################################################


#
#
# # Voto Pr 25
# bd_voto_pr<-
#   bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(voto_pr,pesos) |>
#   filter(!is.na(voto_pr)) |>
#   count(voto_pr,wt = pesos) |>
#   mutate(media = n /sum(n)) |>
#   rename(respuesta=voto_pr)
#
#
# p_voto_pr_graf<-
#   bd_voto_pr|>
#   graficar_barras(salto = 35,
#                   text_size = 6,
#                   porcentajes_fuera = TRUE,
#                   desplazar_porcentajes = 0.05)+
#   #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
#   scale_fill_manual(values = colores_voto_pr ) +
#   scale_y_continuous(limits = c(0, 0.5),
#                      labels = scales::percent) +
#   labs(caption = p_voto_pr_tit)+
#   tema_morant()+
#   theme(axis.text.x = element_text(size = 16),
#         plot.caption = element_text(size = 12))
#
#
# # Voto segundo Pr 25
# bd_voto2_pr<-
#   bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(voto2_pr,pesos) |>
#   filter(!is.na(voto2_pr)) |>
#   count(voto2_pr,wt = pesos) |>
#   mutate(media = n /sum(n)) |>
#   rename(respuesta=voto2_pr)
#
#
# p_voto2_pr_graf<-
#   bd_voto2_pr|>
#   graficar_barras(salto = 35,
#                   text_size = 6,
#                   porcentajes_fuera = TRUE,
#                   desplazar_porcentajes = 0.05)+
#   #graficar_barras(orden_respuestas = rev(orden_voto2_pr))+
#   labs(caption = p_voto2_pr_tit)+
#   scale_fill_manual(values = colores_voto2_pr ) +
#   scale_y_continuous(limits = c(0, 0.5),
#                      labels = scales::percent) +
#   tema_morant()+
#   theme(axis.text.x = element_text(size = 16),
#         plot.caption = element_text(size = 12))
#
#
# # Cualidad mas valorada
#
# bd_cualidades_valora_candidato <-
#   bd_respuestas_efectivas |>
#   tibble::rownames_to_column() %>%
#   as_tibble() |>
#   select(rowname, contains("cualidades_valora_candidato_O"),pesos) |>
#   mutate(tot_pesos = sum(pesos)) |>
#   tidyr::pivot_longer(cols = !c(rowname,pesos,tot_pesos)) |>
#   filter(!is.na(value)) %>%
#   mutate(seleccion = 1) %>%
#   select(-name) %>%
#   tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
#   select(-rowname) %>%
#   summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
#             tot_pesos = unique(tot_pesos) ) %>%
#   tidyr::pivot_longer(-tot_pesos, names_to = "respuesta",values_to = "value") %>%
#   mutate(pct = value/tot_pesos,
#          respuesta = forcats::fct_reorder(.f = respuesta,
#                                           .x = pct,
#                                           .fun = max))
#
#
# p_cualidades_valora_candidato_graf <-
#   bd_cualidades_valora_candidato |>
#   graficar_lollipops(width_cats = 50) +
#   scale_color_manual(values = colores_cualidades_valora_candidato) +
#   scale_y_continuous(limits = c(0, 1.0),
#                      labels = scales::percent) +
#   labs(caption = p_cualidades_valora_candidato_tit) +
#   tema_morant() +
#   theme(axis.text.x = element_text(size = 12),
#         plot.caption = element_text(size = 12))

#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
##### Pres
#*#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


#################################################
#################################################
#################################################
# Opinion buena mala
#################################################
#################################################
#################################################
add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Perfiles segun la opinión de  Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = sexo_gen_op_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Distribución de sexo y edad por opinión de Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_satisfaccion_democracia_op_ominami, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Percepción de la democarcia de los chilenos según su opinión de Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_voto_pr_op_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Preferencia de voto según la opinión de Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_voto2_pr_op_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Segunda preferencia de voto según la opinión de Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_candidato_nunca_voto_op_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Candidato por el que nunca votaría según la opinión de Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))

#
add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
  ph_with(value = p_cualidades_valora_candidato_op_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Cualidades candidatos según la opinión de Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))


#################################################
#################################################
#################################################
# Nunaca Voto
#################################################
#################################################
#################################################
# add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
#   ph_with(value = 'Cualidades de un candidato ',
#           location = ph_location_label(ph_label = "titulo"))
#
#
#
#
#
#
# #
# add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
#   ph_with(value = p_cualidades_valora_candidato_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
#   ph_with(value = 'Cualidades candidatos',
#           location = ph_location_label(ph_label = "titulo"))
