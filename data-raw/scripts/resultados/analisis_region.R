
# nombres region
nombres_region_vec <-
bd_respuestas_efectivas |>
  select(region) |>
  distinct() |>
  filter(!is.na(region)) |>
  pull()



# conocimiento por region
bd_region_conoce_per_per <-
  paste0("conoce_per_",aspectos_conoce_per) |>
  purrr::map_df(.f = ~{
    bd_respuestas_efectivas |>
      select(region,!!rlang::sym(.x),pesos) |>
      count(region,!!rlang::sym(.x),wt = pesos) |>
      filter(!is.na(!!rlang::sym(.x))) |>
      filter(!is.na(region)) |>
      group_by(region) |>
      mutate(media = n /sum(n)) |>
      mutate(aspecto = .x ) |>
      left_join(diccionario |>
                  select(llave,tema),
                by = c('aspecto' = 'llave' )) |>
      rename("respuesta" = .x )
  }
  ) |> ungroup()


bd_region_conoce_per_per<-
  bd_region_conoce_per_per |>
  left_join(bd_conoce_per |>
              filter(respuesta== "Sí") |>
              mutate(media_cono = scales::percent(media,accuracy = 1.0 )) |>
              select(tema, media_cono),
            by="tema") |>
  mutate(tema = paste0(tema," (",media_cono,")"))

# bd_region_redux_pct<-
#   bd_respuestas_efectivas |>
#   mutate(region = case_match(region,
#                                        c("Muy interesado","Interesado")~"INTERESADOS",
#                                        c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
#                                        "Neutral/Indiferente" ~ "NEUTRALES",
#                                        .default = region
#   )) |>
#   count(region,wt = pesos) |>
#   mutate(media_interes =  n/sum(n),
#          media_interes = scales::percent(media_interes,accuracy = 1.0 )) |>
#   select(-n)


candidatos_pr_chile_vec<- bd_region_conoce_per_per |>
  arrange(media_cono) |>
  distinct(tema) |>
  pull()


region_conoce_per_per_1_graf<-
  bd_region_conoce_per_per |>
  filter(respuesta == "Sí") |>
  select(-respuesta) |>
  rename(respuesta=tema) |>
  filter(region != "Ns/Nc") |>
  # left_join(bd_region_redux_pct,
  #           by = "region" ) |>
  # mutate(region = paste0(region," (",media_interes,")")) |>
  filter(region %in% nombres_region_vec[1:4]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  graficar_barras(salto = 25,orden_respuestas = candidatos_pr_chile_vec)+
  scale_fill_manual(values=rep(color_general,9))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~region,nrow = 1)+
  labs(caption = "Conocimiento de los aspirantes \nNota: El porcentaje a ladao de cada candidato, corresponde a su nivel de conocimiento general")+
  tema_morant()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

region_conoce_per_per_2_graf<-
  bd_region_conoce_per_per |>
  filter(respuesta == "Sí") |>
  select(-respuesta) |>
  rename(respuesta=tema) |>
  filter(region != "Ns/Nc") |>
  # left_join(bd_region_redux_pct,
  #           by = "region" ) |>
  # mutate(region = paste0(region," (",media_interes,")")) |>
  filter(region %in% nombres_region_vec[5:8]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  graficar_barras(salto = 25,orden_respuestas = candidatos_pr_chile_vec)+
  scale_fill_manual(values=rep(color_general,9))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~region,nrow = 1)+
  labs(caption = "Conocimiento de los aspirantes \nNota: El porcentaje a ladao de cada candidato, corresponde a su nivel de conocimiento general")+
  tema_morant()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

region_conoce_per_per_3_graf<-
  bd_region_conoce_per_per |>
  filter(respuesta == "Sí") |>
  select(-respuesta) |>
  rename(respuesta=tema) |>
  filter(region != "Ns/Nc") |>
  # left_join(bd_region_redux_pct,
  #           by = "region" ) |>
  # mutate(region = paste0(region," (",media_interes,")")) |>
  filter(region %in% nombres_region_vec[9:12]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  graficar_barras(salto = 25,orden_respuestas = candidatos_pr_chile_vec)+
  scale_fill_manual(values=rep(color_general,9))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~region,nrow = 1)+
  labs(caption = "Conocimiento de los aspirantes \nNota: El porcentaje a ladao de cada candidato, corresponde a su nivel de conocimiento general")+
  tema_morant()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

region_conoce_per_per_4_graf<-
  bd_region_conoce_per_per |>
  filter(respuesta == "Sí") |>
  select(-respuesta) |>
  rename(respuesta=tema) |>
  filter(region != "Ns/Nc") |>
  # left_join(bd_region_redux_pct,
  #           by = "region" ) |>
  # mutate(region = paste0(region," (",media_interes,")")) |>
  filter(region %in% nombres_region_vec[13:16]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  graficar_barras(salto = 25,orden_respuestas = candidatos_pr_chile_vec)+
  scale_fill_manual(values=rep(color_general,9))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~region,nrow = 1)+
  labs(caption = "Conocimiento de los aspirantes \nNota: El porcentaje a ladao de cada candidato, corresponde a su nivel de conocimiento general")+
  tema_morant()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))


# Opinion por region
bd_region_opinion_per <-
  paste0("opinion_",aspectos_conoce_per) |>
  purrr::map_df(.f = ~{
    bd_respuestas_efectivas |>
      select(region,!!rlang::sym(.x),pesos) |>
      count(region,!!rlang::sym(.x),wt = pesos) |>
      filter(!is.na(!!rlang::sym(.x))) |>
      filter(!is.na(region)) |>
      group_by(region) |>
      mutate(media = n /sum(n)) |>
      mutate(aspecto = .x ) |>
      left_join(diccionario |>
                  select(llave,tema),
                by = c('aspecto' = 'llave' )) |>
      rename("respuesta" = .x )
  }
  ) |> ungroup()


bd_region_opinion_per<-
  bd_region_opinion_per |>
  left_join(bd_conoce_per |>
              filter(respuesta== "Sí") |>
              mutate(media_cono = scales::percent(media,accuracy = 1.0 )) |>
              select(tema, media_cono),
            by="tema") |>
  mutate(tema = paste0(tema," (",media_cono,")"))

# bd_region_redux_pct<-
#   bd_respuestas_efectivas |>
#   mutate(region = case_match(region,
#                                        c("Muy interesado","Interesado")~"INTERESADOS",
#                                        c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
#                                        "Neutral/Indiferente" ~ "NEUTRALES",
#                                        .default = region
#   )) |>
#   count(region,wt = pesos) |>
#   mutate(media_interes =  n/sum(n),
#          media_interes = scales::percent(media_interes,accuracy = 1.0 )) |>
#   select(-n)


# candidatos_pr_chile_vec<- bd_region_opinion_per |>
#   distinct(tema) |>
#   pull()


region_opinion_per_1_graf<-
  bd_region_opinion_per |>
  filter(respuesta == "Positiva") |>
  select(-respuesta) |>
  rename(respuesta=tema) |>
  filter(region != "Ns/Nc") |>
  filter(region %in% nombres_region_vec[1:4]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  # left_join(bd_region_redux_pct,
  #           by = "region" ) |>
  # mutate(region = paste0(region," (",media_interes,")")) |>
  graficar_barras(salto = 25,orden_respuestas = candidatos_pr_chile_vec)+
  scale_fill_manual(values=rep(color_opinion_muyBuena,9))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~region,nrow = 1)+
  labs(caption = "Opinión positiva de los aspirantes \nNota: El porcentaje a ladao de cada candidato, corresponde a su nivel de conocimiento general")+
  tema_morant()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

region_opinion_per_2_graf<-
  bd_region_opinion_per |>
  filter(respuesta == "Positiva") |>
  select(-respuesta) |>
  rename(respuesta=tema) |>
  filter(region != "Ns/Nc") |>
  filter(region %in% nombres_region_vec[5:8]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  # left_join(bd_region_redux_pct,
  #           by = "region" ) |>
  # mutate(region = paste0(region," (",media_interes,")")) |>
  graficar_barras(salto = 25,orden_respuestas = candidatos_pr_chile_vec)+
  scale_fill_manual(values=rep(color_opinion_muyBuena,9))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~region,nrow = 1)+
  labs(caption = "Opinión positiva de los aspirantes \nNota: El porcentaje a ladao de cada candidato, corresponde a su nivel de conocimiento general")+
  tema_morant()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

region_opinion_per_3_graf<-
  bd_region_opinion_per |>
  filter(respuesta == "Positiva") |>
  select(-respuesta) |>
  rename(respuesta=tema) |>
  filter(region != "Ns/Nc") |>
  filter(region %in% nombres_region_vec[9:12]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  # left_join(bd_region_redux_pct,
  #           by = "region" ) |>
  # mutate(region = paste0(region," (",media_interes,")")) |>
  graficar_barras(salto = 25,orden_respuestas = candidatos_pr_chile_vec)+
  scale_fill_manual(values=rep(color_opinion_muyBuena,9))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~region,nrow = 1)+
  labs(caption = "Opinión positiva de los aspirantes \nNota: El porcentaje a ladao de cada candidato, corresponde a su nivel de conocimiento general")+
  tema_morant()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

region_opinion_per_4_graf<-
  bd_region_opinion_per |>
  filter(respuesta == "Positiva") |>
  select(-respuesta) |>
  rename(respuesta=tema) |>
  filter(region != "Ns/Nc") |>
  filter(region %in% nombres_region_vec[13:16]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  # left_join(bd_region_redux_pct,
  #           by = "region" ) |>
  # mutate(region = paste0(region," (",media_interes,")")) |>
  graficar_barras(salto = 25,orden_respuestas = candidatos_pr_chile_vec)+
  scale_fill_manual(values=rep(color_opinion_muyBuena,9))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~region,nrow = 1)+
  labs(caption = "Opinión positiva de los aspirantes \nNota: El porcentaje a ladao de cada candidato, corresponde a su nivel de conocimiento general")+
  tema_morant()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

#########
# inclin_op_ominami<-
#   bd_opinion_ominami_voto_proximas_elecciones |>
#   ungroup() |>
#   filter(respuesta == "Positiva") |>
#   top_n(n = 3,wt = media) |>
#   pull(tema)



#Voto proximas elecciones
bd_region_voto_pr_ominami<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr,region,pesos) |>
  filter(!is.na(voto_pr)) |>
  filter(!is.na(region)) |>
  count(voto_pr,region,wt = pesos) |>
  group_by(region) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_pr)


vec_voto_pr_cand <- bd_voto_pr |>
  arrange(media) |>
  pull(respuesta)



p_region_voto_pr_ominami_1_graf<-
  bd_region_voto_pr_ominami|>
  filter(region %in% nombres_region_vec[1:4]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  graficar_barras(salto = 35,
                  text_size = 5,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.06,
                  orden_respuestas = vec_voto_pr_cand)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, .5),
                     labels = scales::percent) +
  labs(caption = paste0("Voto a la presidencia de Chile"))+
  tema_morant()+
  facet_wrap(~region,nrow = 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14))

p_region_voto_pr_ominami_2_graf<-
  bd_region_voto_pr_ominami|>
  filter(region %in% nombres_region_vec[5:8]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  graficar_barras(salto = 35,
                  text_size = 5,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.06,
                  orden_respuestas = vec_voto_pr_cand)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, .5),
                     labels = scales::percent) +
  labs(caption = paste0("Voto a la presidencia de Chile"))+
  tema_morant()+
  facet_wrap(~region,nrow = 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14))


p_region_voto_pr_ominami_3_graf<-
  bd_region_voto_pr_ominami|>
  filter(region %in% nombres_region_vec[9:12]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  graficar_barras(salto = 35,
                  text_size = 5,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.06,
                  orden_respuestas = vec_voto_pr_cand)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, .5),
                     labels = scales::percent) +
  labs(caption = paste0("Voto a la presidencia de Chile"))+
  tema_morant()+
  facet_wrap(~region,nrow = 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14))


p_region_voto_pr_ominami_4_graf<-
  bd_region_voto_pr_ominami|>
  filter(region %in% nombres_region_vec[13:16]) |>
  mutate(region =  stringr::str_wrap(region,width = 15)) |>
  graficar_barras(salto = 35,
                  text_size = 5,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.06,
                  orden_respuestas = vec_voto_pr_cand)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, .5),
                     labels = scales::percent) +
  labs(caption = paste0("Voto a la presidencia de Chile"))+
  tema_morant()+
  facet_wrap(~region,nrow = 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14))





#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
##### Pres
#*#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*
#*

library(officer)

path_export <-
  encuestar:::formato_archivo(nombre = "./data-raw/press/analisis_region",
                              extension = "pptx",
                              tolerancia = 60)

pptx <-
  read_pptx(path = "./data-raw/plantilla_general_09_12_24.pptx")

# Conocimiento
add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Análisis de conocimiento por región de los aspirantes a la presidencia de Chile',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = region_conoce_per_per_1_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Conocimiento de los aspirantes a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = region_conoce_per_per_2_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Conocimiento de los aspirantes a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = region_conoce_per_per_3_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Conocimiento de los aspirantes a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = region_conoce_per_per_4_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Conocimiento de los aspirantes a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

# Opinión Buena
add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Análisis de opinión por región de los aspirantes a la presidencia de Chile',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = region_opinion_per_1_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Opinión de los aspirantes a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = region_opinion_per_2_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Opinión de los aspirantes a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = region_opinion_per_3_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Opinión de los aspirantes a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = region_opinion_per_4_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Opinión de los aspirantes a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

# Intención de voto
add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Análisis de intención de voto por región para los aspirantes a la presidencia de Chile',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_region_voto_pr_ominami_1_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Intención de voto a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_region_voto_pr_ominami_2_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Intención de voto a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_region_voto_pr_ominami_3_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Intención de voto a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_region_voto_pr_ominami_4_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Intención de voto a la presidencia de Chile en la Región ...',
          location = ph_location_label(ph_label = "titulo"))



print(pptx, path_export)

