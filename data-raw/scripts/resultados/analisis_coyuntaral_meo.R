#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#--------Punto 1--------
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-

bd_conoce_ominami_voto_pr_bac_vo <-
    bd_respuestas_efectivas |>
      select(voto_pr,all_of("conoce_per_ominami"),pesos) |>
      count(voto_pr,conoce_per_ominami,wt = pesos) |>
      filter(!is.na(voto_pr)) |>
      group_by(voto_pr) |>
      mutate(media = n /sum(n)) |>
      mutate(aspecto = "conoce_per_ominami") |>
      left_join(diccionario |>
                  select(llave,tema),
                by = c('aspecto' = 'llave' )) |>
      rename(respuesta = conoce_per_ominami )|>
      filter(voto_pr %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
      mutate(tema = paste0("Electorado de \n",voto_pr))



bd_opinion_ominami_voto_pr_bac_vo <-
bd_respuestas_efectivas |>
  select(voto_pr,opinion_ominami,pesos) |>
  count(voto_pr,opinion_ominami,wt = pesos) |>
  filter(!is.na(opinion_ominami)) |>
  filter(!is.na(voto_pr)) |>
  group_by(voto_pr) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "opinion_ominami" ) |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = "opinion_ominami" ) |>
  filter(voto_pr %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0("Electorado de \n",voto_pr))



p_opinion_ominami_voto_pr_bac_vo_graf<-
  bd_opinion_ominami_voto_pr_bac_vo |>
  graficar_candidato_opinion(

    #patron_inicial = "opinion",
    #aspectos = aspectos_conoce_per,
    size_text_cat = 16,

    #OPINION
    salto = 45,
    colores = colores_opinion_per,
    regular = "",
    orden_resp = c("Negativa","Positiva"),
    grupo_positivo = c("Positiva"),
    grupo_negativo = rev(c("Negativa")),
    caption_opinion  ="Opinion de Marco Enríquez-Ominami, por electorado" ,
    size_caption_opinion = 12,

    #CONOCIMIENTO
    burbuja = bd_conoce_ominami_voto_pr_bac_vo |> filter(respuesta == 'Sí') |> rename(valor =  respuesta),
    #llave_burbuja = "conoce_per",
    color_burbuja = color_general,
    caption_burbuja = "Conocimiento",
    size_caption_burbuja = 12,
    size_burbuja = 8,

    #NO SABE NO CONTESTA
    ns_nc ="Ns/Nc (No leer)",
    caption_nsnc = "Ns/Nc",
    size_caption_nsnc = 12,
    color_nsnc = "gray50"

  )
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#--------Punto 2--------
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-


################################################
# opinion de ominami por inclinacion voto
################################################

bd_conoce_ominami_voto_proximas_elecciones <-
  bd_respuestas_efectivas |>
  select(voto_proximas_elecciones,all_of("conoce_per_ominami"),pesos) |>
  count(voto_proximas_elecciones,conoce_per_ominami,wt = pesos) |>
  filter(!is.na(voto_proximas_elecciones)) |>
  group_by(voto_proximas_elecciones) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "conoce_per_ominami") |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = conoce_per_ominami )|>
  #filter(voto_proximas_elecciones %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0(voto_proximas_elecciones))



bd_opinion_ominami_voto_proximas_elecciones <-
  bd_respuestas_efectivas |>
  select(voto_proximas_elecciones,opinion_ominami,pesos) |>
  count(voto_proximas_elecciones,opinion_ominami,wt = pesos) |>
  filter(!is.na(opinion_ominami)) |>
  filter(!is.na(voto_proximas_elecciones)) |>
  group_by(voto_proximas_elecciones) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "opinion_ominami" ) |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = "opinion_ominami" ) |>
  #filter(voto_proximas_elecciones %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0(voto_proximas_elecciones))



p_opinion_ominami_voto_proximas_elecciones_graf<-
  bd_opinion_ominami_voto_proximas_elecciones |>
  graficar_candidato_opinion(

    #patron_inicial = "opinion",
    #aspectos = aspectos_conoce_per,
    size_text_cat = 16,

    #OPINION
    salto = 45,
    colores = colores_opinion_per,
    regular = "",
    orden_resp = c("Negativa","Positiva"),
    grupo_positivo = c("Positiva"),
    grupo_negativo = rev(c("Negativa")),
    caption_opinion  ="Opinion de Marco Enríquez-Ominami, por inclinación al voto" ,
    size_caption_opinion = 12,

    #CONOCIMIENTO
    burbuja = bd_conoce_ominami_voto_proximas_elecciones |> filter(respuesta == 'Sí') |> rename(valor =  respuesta),
    #llave_burbuja = "conoce_per",
    color_burbuja = color_general,
    caption_burbuja = "Conocimiento",
    size_caption_burbuja = 12,
    size_burbuja = 8,

    #NO SABE NO CONTESTA
    ns_nc ="Ns/Nc (No leer)",
    caption_nsnc = "Ns/Nc",
    size_caption_nsnc = 12,
    color_nsnc = "gray50"

  )



################################################
# voto pr perfil
################################################

inclin_op_ominami<-
  bd_opinion_ominami_voto_proximas_elecciones |>
  ungroup() |>
  filter(respuesta == "Positiva") |>
  top_n(n = 3,wt = media) |>
  pull(tema)



#Voto proximas elecciones
bd_voto_proximas_elecciones_voto_pr_ominami<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr,voto_proximas_elecciones,pesos) |>
  filter(!is.na(voto_pr)) |>
  count(voto_pr,voto_proximas_elecciones,wt = pesos) |>
  group_by(voto_proximas_elecciones) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_pr)


p_voto_proximas_elecciones_voto_pr_ominami_graf<-
  bd_voto_proximas_elecciones_voto_pr_ominami|>
  filter(voto_proximas_elecciones %in% inclin_op_ominami) |>
  mutate(voto_proximas_elecciones =  stringr::str_wrap(voto_proximas_elecciones,width = 15)) |>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.05)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, .75),
                     labels = scales::percent) +
  labs(caption = paste0("Voto a la presidencia de Chile","\nTres principales perfiles de inclinación al voto \nque tinen percepción positiva de Ominami"))+
  tema_morant()+
  facet_wrap(~voto_proximas_elecciones)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#--------Punto 3--------
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-


################################################
# voto pr opinion ominami
################################################

bd_op_ominami <-
  bd_respuestas_efectivas |>
  filter(conoce_per_ominami == "Sí") |>
  count(opinion_ominami,wt = pesos) |>
  mutate(coef_op = n/sum(n),
         coef_op =  scales::percent(coef_op,accuracy =1.0)) |>
  select(-n)

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

#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#--------Punto 4-----------------
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-

bd_chile_actual_ominami <-
  bd_respuestas_efectivas |>
  filter(conoce_per_ominami == "Sí") |>
  count(chile_actual,wt = pesos) |>
  mutate(coef_op = n/sum(n),
         coef_op =  scales::percent(coef_op,accuracy =1.0)) |>
  select(-n)

# Voto Pr 25
bd_voto_pr_chile_actual<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  filter(chile_actual != "Ns/Nc") |>
  #mutate(chile_actual = ifelse(chile_actual == "Ns/Nc (No leer)","Ns/Nc",chile_actual )) |>
  select(chile_actual,voto_pr,pesos) |>
  filter(!is.na(voto_pr)) |>
  group_by(chile_actual) |>
  count(chile_actual,voto_pr,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_pr) %>%
  left_join(bd_chile_actual_ominami,by = "chile_actual") |>
  mutate(chile_actual = paste0(chile_actual," (",coef_op,")"))


p_voto_pr_chile_actual_graf<-
  bd_voto_pr_chile_actual|>
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
  facet_wrap(~chile_actual)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))

#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#--------Punto 5--------
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-

################################################
# sexo edad interés política
################################################

bd_interes_politica_mod<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(interes_politica,pesos) |>
  mutate(interes_politica = case_match(interes_politica,
                                       c("Muy interesado","Interesado")~"INTERESADOS",
                                       c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
                                       "Neutral/Indiferente" ~ "NEUTRALES",
                                       .default = interes_politica
  )) |>
  count(interes_politica,wt = pesos) |>
  mutate(coef_op =  n/sum(n),
         coef_op =  scales::percent(coef_op,accuracy =1.0)) |>
  select(-n)

sexo_gen_interes_politica_graf<-
  #base
  bd_respuestas_efectivas |>
  mutate(interes_politica = case_match(interes_politica,
                                       c("Muy interesado","Interesado")~"INTERESADOS",
                                       c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
                                       "Neutral/Indiferente" ~ "NEUTRALES",
                                       .default = interes_politica
  )) |>
  filter(!sexo == "-") |>
  filter(!interes_politica %in% c("Ns/Nc","NEUTRALES")) |>
  #mutate(interes_politica = ifelse(interes_politica == "Ns/Nc (No leer)","Ns/Nc",interes_politica )) |>
  count(interes_politica,sexo,generacion,wt = pesos) |>
  group_by(interes_politica) |>
  mutate(coef = n/sum(n)) |>
  ungroup() |>
  mutate(coef = case_when(sexo == "Hombre" ~ -coef,
                          T  ~ coef)) %>%
  left_join(bd_interes_politica_mod,by = "interes_politica") |>
  mutate(interes_politica = paste0(interes_politica," (",coef_op,")")) |>
  #grafica
  ggplot(aes(x = generacion, y = coef, fill = sexo, label = scales::percent(abs(coef), 1))) +
  ggchicklet::geom_chicklet(alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c(color_h, color_m)) +
  ggfittext::geom_bar_text(contrast = TRUE) +
  lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
  tema_morant() +
  #division
  facet_wrap(~interes_politica)+
  theme(legend.title = element_blank(),
        axis.title.y = element_text(family = font_family, colour = font_color, size = 16),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        plot.background = element_rect(color = "transparent",fill = "transparent"),
        panel.background = element_rect(color = "transparent",fill = "transparent")) +
  labs(x = "Rango de edad",y=NULL,
       caption = 'Rango de edad y sexo de las personas \nrespecto a su interés en la política')




################################################
# ingreso interés política
################################################

bd_ingreso_mensual_hogar_interes_politica <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  mutate(interes_politica = case_match(interes_politica,
                                       c("Muy interesado","Interesado")~"INTERESADOS",
                                       c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
                                       "Neutral/Indiferente" ~ "NEUTRALES",
                                       .default = interes_politica
  )) |>
  group_by(interes_politica) |>
  count(interes_politica,ingreso_mensual_hogar,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  ungroup() |>
  rename(respuesta = ingreso_mensual_hogar) %>%
  filter(!interes_politica %in% c("Ns/Nc","NEUTRALES")) |>
  left_join(bd_interes_politica_mod,by = "interes_politica") |>
  mutate(interes_politica = paste0(interes_politica," (",coef_op,")"))

g_ingreso_mensual_hogarinteres_politica <-
  bd_ingreso_mensual_hogar_interes_politica |>
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
  facet_wrap(~interes_politica)+
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))



################################################
# educacion interés política
################################################

bd_curso_aprobado_interes_politica <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  mutate(interes_politica = case_match(interes_politica,
                                       c("Muy interesado","Interesado")~"INTERESADOS",
                                       c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
                                       "Neutral/Indiferente" ~ "NEUTRALES",
                                       .default = interes_politica
  )) |>
  group_by(interes_politica) |>
  count(interes_politica,curso_aprobado,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  ungroup() |>
  rename(respuesta = curso_aprobado) %>%
  filter(!interes_politica %in% c("Ns/Nc","NEUTRALES")) |>
  left_join(bd_interes_politica_mod,by = "interes_politica") |>
  mutate(interes_politica = paste0(interes_politica," (",coef_op,")"))

g_curso_aprobadointeres_politica <-
  bd_curso_aprobado_interes_politica |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = orden_curso_aprobado) +
  scale_fill_manual(values = colores_curso_aprobado) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  labs(caption = p_curso_aprobado_tit) +
  tema_morant() +
  facet_wrap(~interes_politica)+
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))


################################################
# voto pr interés política
################################################



#Voto proximas elecciones Chile actual
bd_interes_politica_voto_pr_ominami<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr,interes_politica,pesos) |>
  filter(!is.na(voto_pr)) |>
  mutate(interes_politica = case_match(interes_politica,
                                      c("Muy interesado","Interesado")~"INTERESADOS",
                                      c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
                                      "Neutral/Indiferente" ~ "NEUTRALES",
                                      .default = interes_politica
                                      )) |>
  count(voto_pr,interes_politica,wt = pesos) |>
  group_by(interes_politica) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_pr) %>%
  filter(!interes_politica %in% c("Ns/Nc","NEUTRALES")) |>
  left_join(bd_interes_politica_mod,by = "interes_politica") |>
  mutate(interes_politica = paste0(interes_politica," (",coef_op,")"))


p_interes_politica_voto_pr_ominami_graf<-
  bd_interes_politica_voto_pr_ominami|>
  mutate(interes_politica =  stringr::str_wrap(interes_politica,width = 15)) |>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.05)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, .75),
                     labels = scales::percent) +
  labs(caption = paste0("Voto a la presidencia de Chile","\npor Interés en la política"))+
  tema_morant()+
  facet_wrap(~interes_politica)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



################################################
# analisis correspondencia interés política
################################################


voto_pr_interes_politica_ac<-
  encuestar::analisis_correspondencia(var1 = "interes_politica",
                                      var2 = "voto_pr",
                                      legenda1 = "Interés en la política",
                                      legenda2 = "Preferencia de voto",
                                      diseno = calibrated_design)+
  tema_transparente()



################################################
# interés política  postura idelógica
################################################

# postura ideologica
bd_definicion_postura_ideologica_interes_plolitica<-
  bd_respuestas_efectivas |>
  mutate(interes_politica = case_match(interes_politica,
                                       c("Muy interesado","Interesado")~"INTERESADOS",
                                       c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
                                       "Neutral/Indiferente" ~ "NEUTRALES",
                                       .default = interes_politica
  )) |>
  as_tibble() |>
  select(interes_politica,definicion_postura_ideologica,pesos) |>
  #filter(!is.na(definicion_postura_ideologica)) |>
  count(interes_politica,definicion_postura_ideologica,wt = pesos) |>
  group_by(interes_politica) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=definicion_postura_ideologica)%>%
  filter(!interes_politica %in% c("Ns/Nc","NEUTRALES")) |>
  left_join(bd_interes_politica_mod,by = "interes_politica") |>
  mutate(interes_politica = paste0(interes_politica," (",coef_op,")"))



p_definicion_postura_ideologica_interes_plolitica_graf<-
  bd_definicion_postura_ideologica_interes_plolitica|>
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
  facet_wrap(~interes_politica)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



################################################
# interés política  opinion ominami
################################################


bd_conoce_ominami_interes_politica <-
  bd_respuestas_efectivas |>
  mutate(interes_politica = case_match(interes_politica,
                                       c("Muy interesado","Interesado")~"INTERESADOS",
                                       c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
                                       "Neutral/Indiferente" ~ "NEUTRALES",
                                       .default = interes_politica
  )) |>
  select(interes_politica,all_of("conoce_per_ominami"),pesos) |>
  count(interes_politica,conoce_per_ominami,wt = pesos) |>
  filter(!is.na(interes_politica)) |>
  group_by(interes_politica) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "conoce_per_ominami") |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = conoce_per_ominami )|>
  #filter(interes_politica %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0(interes_politica))



bd_opinion_ominami_interes_politica <-
  bd_respuestas_efectivas |>
  mutate(interes_politica = case_match(interes_politica,
                                       c("Muy interesado","Interesado")~"INTERESADOS",
                                       c("Nada interesado","Muy poco interesado")~"NO INTERESADOS",
                                       "Neutral/Indiferente" ~ "NEUTRALES",
                                       .default = interes_politica
  )) |>
  select(interes_politica,opinion_ominami,pesos) |>
  count(interes_politica,opinion_ominami,wt = pesos) |>
  filter(!is.na(opinion_ominami)) |>
  filter(!is.na(interes_politica)) |>
  group_by(interes_politica) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "opinion_ominami" ) |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = "opinion_ominami" ) |>
  #filter(interes_politica %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0(interes_politica))



p_opinion_ominami_interes_politica_graf<-
  bd_opinion_ominami_interes_politica |>
  filter(!interes_politica  %in% c("Ns/Nc") ) |>
  graficar_candidato_opinion(

    #patron_inicial = "opinion",
    #aspectos = aspectos_conoce_per,
    size_text_cat = 16,

    #OPINION
    salto = 45,
    colores = colores_opinion_per,
    regular = "",
    orden_resp = c("Negativa","Positiva"),
    grupo_positivo = c("Positiva"),
    grupo_negativo = rev(c("Negativa")),
    caption_opinion  ="Opinion de Marco Enríquez-Ominami, por interés en la política" ,
    size_caption_opinion = 12,

    #CONOCIMIENTO
    burbuja = bd_conoce_ominami_interes_politica |> filter(respuesta == 'Sí') |> filter(!interes_politica  %in% c("Ns/Nc") ) |>  rename(valor =  respuesta),
    #llave_burbuja = "conoce_per",
    color_burbuja = color_general,
    caption_burbuja = "Conocimiento",
    size_caption_burbuja = 12,
    size_burbuja = 8,

    #NO SABE NO CONTESTA
    ns_nc ="Ns/Nc (No leer)",
    caption_nsnc = "Ns/Nc",
    size_caption_nsnc = 12,
    color_nsnc = "gray50"

  )


#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#--------Punto 6--------
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-
#+-+-+-+-++-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-++-

############################################
# Ninguno intencion de voto
############################################



bd_voto_pr_ninguno <-
  bd_respuestas_efectivas |>
  filter(!is.na(voto_pr)) |>
  count(voto_pr,wt = pesos) |>
  mutate(coef_op = n/sum(n),
         coef_op =  scales::percent(coef_op,accuracy =1.0)) |>
  select(-n)



sexo_gen_voto_pr_ninguno_graf<-
  #base
  bd_respuestas_efectivas |>
  filter(!is.na(voto_pr)) |>
  filter(voto_pr == "Ninguno") |>
  filter(!sexo == "-") |>
  filter(voto_pr != "Ns/Nc (No leer)") |>
  #mutate(voto_pr = ifelse(voto_pr == "Ns/Nc (No leer)","Ns/Nc",voto_pr )) |>
  count(voto_pr,sexo,generacion,wt = pesos) |>
  group_by(voto_pr) |>
  mutate(coef = n/sum(n)) |>
  ungroup() |>
  mutate(coef = case_when(sexo == "Hombre" ~ -coef,
                          T  ~ coef)) %>%
  left_join(bd_voto_pr_ninguno,by = "voto_pr") |>
  mutate(voto_pr = paste0(voto_pr," (",coef_op,")")) |>
  #grafica
  ggplot(aes(x = generacion, y = coef, fill = sexo, label = scales::percent(abs(coef), 1))) +
  ggchicklet::geom_chicklet(alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c(color_h, color_m)) +
  ggfittext::geom_bar_text(contrast = TRUE) +
  lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
  tema_morant() +
  #division
  facet_wrap(~voto_pr)+
  theme(legend.title = element_blank(),
        axis.title.y = element_text(family = font_family, colour = font_color, size = 16),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        plot.background = element_rect(color = "transparent",fill = "transparent"),
        panel.background = element_rect(color = "transparent",fill = "transparent")) +
  labs(x = "Rango de edad",y=NULL,
       caption = 'Rango de edad y sexo de las personas \n que no tienen una preferencia electoral')




############################################
# Ninguno intencion de voto
############################################


bd_ingreso_mensual_hogarvoto_pr_ninguno <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  filter(voto_pr == "Ninguno") |>
  count(ingreso_mensual_hogar,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = ingreso_mensual_hogar) |>
  left_join(bd_voto_pr_ninguno,by = "voto_pr") |>
  mutate(voto_pr = paste0(voto_pr," (",coef_op,")"))

g_ingreso_mensual_hogarvoto_pr_ninguno <-
  bd_ingreso_mensual_hogarvoto_pr_ninguno |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = orden_ingreso_mensual_hogar) +
  scale_fill_manual(values = colores_ingreso_mensual_hogar) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = "Ingreso de los Hogares de las personas que no tienen una preferencia electoral") +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))



################################################
# interés política  voto pr ninguno
################################################

# postura ideologica
bd_definicion_postura_ideologica_voto_pr_ninguno<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr,definicion_postura_ideologica,pesos) |>
  #filter(!is.na(definicion_postura_ideologica)) |>
  count(voto_pr,definicion_postura_ideologica,wt = pesos) |>
  group_by(voto_pr) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=definicion_postura_ideologica)%>%
  filter(voto_pr %in% c("Ninguno")) |>
  left_join(bd_voto_pr_ninguno,by = "voto_pr") |>
  mutate(voto_pr = paste0(voto_pr," (",coef_op,")"))



p_definicion_postura_ideologica_voto_pr_ninguno_graf<-
  bd_definicion_postura_ideologica_voto_pr_ninguno|>
  #graficar_barras()+
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(orden_definicion_postura_ideologica))+
  scale_y_continuous(limits = c(0, .75),
                     labels = scales::percent) +
  scale_fill_manual(values = colores_definicion_postura_ideologica) +
  labs(caption = p_definicion_postura_ideologica_tit)+
  tema_morant()+
  facet_wrap(~voto_pr)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))




#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
##### Pres
#*#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-



add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Analisis coyuntural respecto estrategias para  Marco Enríquez-Ominami',
          location = ph_location_label(ph_label = "titulo"))






