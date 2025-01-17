

sexo_rango_edad_graf<-
  #base
  bd_respuestas_efectivas |>
  filter(!sexo == "-") |>
  count(sexo,rango_edad,wt = pesos) |>
  group_by(sexo) |>
  mutate(coef = n/sum(n)) |>
  ungroup() |>
  mutate(coef = case_when(sexo == "Hombre" ~ -coef,
                          T  ~ coef)) %>%
  #grafica
  ggplot(aes(x = rango_edad, y = coef, fill = sexo, label = scales::percent(abs(coef), 1))) +
  ggchicklet::geom_chicklet(alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c(color_h, color_m)) +
  ggfittext::geom_bar_text(contrast = TRUE) +
  lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
  tema_morant() +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(family = font_family, colour = font_color, size = 16),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        plot.background = element_rect(color = "transparent",fill = "transparent"),
        panel.background = element_rect(color = "transparent",fill = "transparent")) +
  labs(x = "Rango de edad",y=NULL,
       caption = 'Rango de edad y sexo de las personas entrevistadas')



#######################################################################
#-----------------Bloque Bachelete
#######################################################################

#Cualidades candidato
bd_cualidades_valora_candidato_voto_pr_bachelet <-
  bd_respuestas_efectivas |>
  filter(voto_pr != "Ns/Nc (No leer)") |>
  #mutate(voto_pr = ifelse(voto_pr == "Ns/Nc (No leer)","Ns/Nc",voto_pr )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,voto_pr ,contains("cualidades_valora_candidato_O"),pesos) |>
  group_by(voto_pr) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,voto_pr,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(voto_pr) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,voto_pr), names_to = "respuesta",values_to = "value") %>%
  group_by(voto_pr) |>
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max))


p_cualidades_valora_candidato_voto_pr_bachelet_graf <-
  bd_cualidades_valora_candidato_voto_pr_bachelet |>
  filter(voto_pr=="Michelle Bachelet") |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_cualidades_valora_candidato) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  tema_morant() +
  labs(caption = "¿Cuál de las siguientes cualidades valora más en un \ncandidato presidencial? Me puede\ndecir tres cualidades, por favor") +
  facet_wrap(~voto_pr)+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))


#problematicas
bd_problematicas_voto_pr_bachelet <-
  bd_respuestas_efectivas |>
  filter(voto_pr != "Ns/Nc (No leer)") |>
  #mutate(voto_pr = ifelse(voto_pr == "Ns/Nc (No leer)","Ns/Nc",voto_pr )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,voto_pr ,contains("problema_chile_O"),pesos) |>
  group_by(voto_pr) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,voto_pr,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  distinct(rowname,voto_pr, pesos, tot_pesos, value,seleccion) |>
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(voto_pr) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,voto_pr), names_to = "respuesta",values_to = "value") %>%
  group_by(voto_pr) |>
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max))


p_problematicas_voto_pr_bachelet_graf <-
  bd_problematicas_voto_pr_bachelet |>
  filter(voto_pr=="Michelle Bachelet") |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_problema) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  tema_morant() +
  labs(caption = "¿Cuales son las principales problemáticas en Chile?\n Para el electorado de Michelle Bachellet ") +
  facet_wrap(~voto_pr)+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))


# medios de comunicacion
bd_medios_com_voto_pr_bachelet <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(voto_pr,medios_com,wt = pesos) |>
  na.omit() |>
  group_by(voto_pr) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = medios_com,
         pct = media) |>
  ungroup()

g_medios_com_voto_pr_bachelet <-
  bd_medios_com_voto_pr_bachelet |>
  filter(voto_pr=="Michelle Bachelet") |>
  graficar_lollipops(width_cats = 35,size_pct = 5) +
  scale_color_manual(values = colores_medios_com) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = "Principales medios de comunicación de los votantes de Michelle Bachellet") +
  tema_morant() +
  facet_wrap(~voto_pr)+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 12))

#######################################################################
#-----------------Bloque Mujeres
#######################################################################
bd_cualidades_valora_candidato_mujeres <-
  bd_respuestas_efectivas |>
  filter(sexo == "Mujer") |>
  #mutate(sexo = ifelse(sexo == "Ns/Nc (No leer)","Ns/Nc",sexo )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,sexo ,contains("cualidades_valora_candidato_O"),pesos) |>
  group_by(sexo) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,sexo,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(sexo) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,sexo), names_to = "respuesta",values_to = "value") %>%
  group_by(sexo) |>
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max))


p_cualidades_valora_candidato_mujeres_graf <-
  bd_cualidades_valora_candidato_mujeres |>
  filter(sexo=="Mujer") |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_cualidades_valora_candidato) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  tema_morant() +
  labs(caption = "¿Cuál de las siguientes cualidades valora más en un \ncandidato presidencial? Me puede\ndecir tres cualidades, por favor") +
  facet_wrap(~sexo)+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))



#Problematicas
bd_problematicas_mujeres <-
  bd_respuestas_efectivas |>
  filter(sexo == "Mujer") |>
  #mutate(sexo = ifelse(sexo == "Ns/Nc (No leer)","Ns/Nc",sexo )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,sexo ,contains("problema_chile_O"),pesos) |>
  group_by(sexo) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,sexo,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  distinct(rowname,sexo, pesos, tot_pesos, value,seleccion) |>
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(sexo) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,sexo), names_to = "respuesta",values_to = "value") %>%
  group_by(sexo) |>
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max))


p_problematicas_mujeres_graf <-
  bd_problematicas_mujeres |>
  filter(sexo=="Mujer") |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_problema) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  tema_morant() +
  labs(caption = "¿Cuales son las principales problemáticas en Chile?\n Para las mujeres") +
  facet_wrap(~sexo)+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))

# medios de comunicacion
bd_medios_com_mujeres <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(sexo,medios_com,wt = pesos) |>
  na.omit() |>
  group_by(sexo) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = medios_com,
         pct = media) |>
  ungroup()

g_medios_com_mujeres <-
  bd_medios_com_mujeres |>
  filter(sexo=="Mujer") |>
  graficar_lollipops(width_cats = 35,size_pct = 5) +
  scale_color_manual(values = colores_medios_com) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = "Principales medios de comunicación de las chilenas") +
  tema_morant() +
  facet_wrap(~sexo)+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 12))





#######################################################################
#-----------------Bloque Huerfanos
#######################################################################



#Los huérfanos políticos: sin interés en la política, independientes y sin opción de votante (hacer cruces para constatar)



huerfanos_pol_graf<-
  bd_respuestas_efectivas |>
  # mutate(huerfano_pol =  ifelse(interes_politica %in% c("Nada interesado","Muy poco interesado") | voto_proximas_elecciones %in% c("Por un candidato independiente")|voto_pr %in% c("Ninguno","Ns/Nc"),
  #                               "Huérfano político","Alguna preferencia" ) ) |>
  mutate(huerfano_pol =  ifelse(identificacion_partido %in% c("Ninguno") & definicion_postura_ideologica %in% c("Ninguno"),
                                "Huérfano político","Alguna preferencia" ) ) |>
  count(huerfano_pol,wt = pesos) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = huerfano_pol) |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.04,
                  orden_respuestas = rev(c("Huérfano político","Alguna preferencia"))) +
  scale_fill_manual(values = rep(color_general,2)) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  labs(caption = paste0("Huérfanos políticos",
                        "\n Conformados por personas sin identificación partidista y sin postura ideológica")) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))


bd_cualidades_valora_candidato_huerfanos_politicos <-
  bd_respuestas_efectivas |>
  # mutate(huerfano_pol =  ifelse(interes_politica %in% c("Nada interesado","Muy poco interesado") | voto_proximas_elecciones %in% c("Por un candidato independiente")|voto_pr %in% c("Ninguno","Ns/Nc"),
  #                               "Huérfano político","Alguna preferencia" ) ) |>
  mutate(huerfano_pol =  ifelse(identificacion_partido %in% c("Ninguno") & definicion_postura_ideologica %in% c("Ninguno"),
                                "Huérfano político","Alguna preferencia" ) ) |>
  filter(huerfano_pol == "Huérfano político") |>
  #mutate(huerfano_pol = ifelse(huerfano_pol == "Ns/Nc (No leer)","Ns/Nc",huerfano_pol )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,huerfano_pol ,contains("cualidades_valora_candidato_O"),pesos) |>
  group_by(huerfano_pol) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,huerfano_pol,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(huerfano_pol) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,huerfano_pol), names_to = "respuesta",values_to = "value") %>%
  group_by(huerfano_pol) |>
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max))


p_cualidades_valora_candidato_huerfanos_politicos_graf <-
  bd_cualidades_valora_candidato_huerfanos_politicos |>
  filter(huerfano_pol=="Huérfano político") |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_cualidades_valora_candidato) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  tema_morant() +
  labs(caption = "¿Cuál de las siguientes cualidades valora más en un \ncandidato presidencial? Me puede\ndecir tres cualidades, por favor") +
  facet_wrap(~huerfano_pol)+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))



#Problematica
bd_problematicas_huerfanos_politicos <-
  bd_respuestas_efectivas |>
  # mutate(huerfano_pol =  ifelse(interes_politica %in% c("Nada interesado","Muy poco interesado") | voto_proximas_elecciones %in% c("Por un candidato independiente")|voto_pr %in% c("Ninguno","Ns/Nc"),
  #                               "Huérfano político","Alguna preferencia" ) ) |>
  mutate(huerfano_pol =  ifelse(identificacion_partido %in% c("Ninguno") & definicion_postura_ideologica %in% c("Ninguno"),
                                "Huérfano político","Alguna preferencia" ) ) |>
  filter(huerfano_pol == "Huérfano político") |>
  #mutate(huerfano_pol = ifelse(huerfano_pol == "Ns/Nc (No leer)","Ns/Nc",huerfano_pol )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,huerfano_pol ,contains("problema_chile_O"),pesos) |>
  group_by(huerfano_pol) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,huerfano_pol,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  distinct(rowname,huerfano_pol, pesos, tot_pesos, value,seleccion) |>
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(huerfano_pol) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,huerfano_pol), names_to = "respuesta",values_to = "value") %>%
  group_by(huerfano_pol) |>
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max))


p_problematicas_huerfanos_politicos_graf <-
  bd_problematicas_huerfanos_politicos |>
  filter(huerfano_pol=="Huérfano político") |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_problema) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  tema_morant() +
  labs(caption = "¿Cuales son las principales problemáticas en Chile?\n Para los huérfanos políticos") +
  facet_wrap(~huerfano_pol)+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))






# medios de comunicacion
bd_medios_com_huerfano_pol <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  # mutate(huerfano_pol =  ifelse(interes_politica %in% c("Nada interesado","Muy poco interesado") | voto_proximas_elecciones %in% c("Por un candidato independiente")|voto_pr %in% c("Ninguno","Ns/Nc"),
  #                               "Huérfano político","Alguna preferencia" ) ) |>
  mutate(huerfano_pol =  ifelse(identificacion_partido %in% c("Ninguno") & definicion_postura_ideologica %in% c("Ninguno"),
                                "Huérfano político","Alguna preferencia" ) ) |>
  count(huerfano_pol,medios_com,wt = pesos) |>
  na.omit() |>
  group_by(huerfano_pol) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = medios_com,
         pct = media) |>
  ungroup()

g_medios_com_huerfano_pol <-
  bd_medios_com_huerfano_pol |>
  filter(huerfano_pol=="Huérfano político") |>
  graficar_lollipops(width_cats = 35,size_pct = 5) +
  scale_color_manual(values = colores_medios_com) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = "Principales medios de comunicación de los Huerfanos políticos") +
  tema_morant() +
  facet_wrap(~huerfano_pol)+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 12))


#####################################################################################################3


################################################
# opinion de ominami por sexo
################################################

bd_conoce_ominami_sexo <-
  bd_respuestas_efectivas |>
  select(sexo,all_of("conoce_per_ominami"),pesos) |>
  count(sexo,conoce_per_ominami,wt = pesos) |>
  filter(!is.na(sexo)) |>
  filter(sexo!="-") |>
  group_by(sexo) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "conoce_per_ominami") |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = conoce_per_ominami )|>
  #filter(sexo %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0(sexo))



bd_opinion_ominami_sexo <-
  bd_respuestas_efectivas |>
  select(sexo,opinion_ominami,pesos) |>
  count(sexo,opinion_ominami,wt = pesos) |>
  filter(!is.na(opinion_ominami)) |>
  filter(!is.na(sexo)) |>
  filter(sexo!="-") |>
  group_by(sexo) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "opinion_ominami" ) |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = "opinion_ominami" ) |>
  #filter(sexo %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0(sexo))



p_opinion_ominami_sexo_graf<-
  bd_opinion_ominami_sexo |>
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
    caption_opinion  ="Opinion de Marco Enríquez-Ominami, por Sexo" ,
    size_caption_opinion = 12,

    #CONOCIMIENTO
    burbuja = bd_conoce_ominami_sexo |> filter(respuesta == 'Sí') |> rename(valor =  respuesta),
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
# opinion de ominami por huerfanos
################################################

bd_conoce_ominami_huerfano_pol <-
  bd_respuestas_efectivas |>
  mutate(huerfano_pol =  ifelse(identificacion_partido %in% c("Ninguno") & definicion_postura_ideologica %in% c("Ninguno"),
                                "Huérfano político","Alguna preferencia" ) ) |>
  select(huerfano_pol,all_of("conoce_per_ominami"),pesos) |>
  count(huerfano_pol,conoce_per_ominami,wt = pesos) |>
  filter(!is.na(huerfano_pol)) |>
  filter(huerfano_pol!="-") |>
  group_by(huerfano_pol) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "conoce_per_ominami") |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = conoce_per_ominami )|>
  #filter(huerfano_pol %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0(huerfano_pol))



bd_opinion_ominami_huerfano_pol <-
  bd_respuestas_efectivas |>
  mutate(huerfano_pol =  ifelse(identificacion_partido %in% c("Ninguno") & definicion_postura_ideologica %in% c("Ninguno"),
                                "Huérfano político","Alguna preferencia" ) ) |>
  select(huerfano_pol,opinion_ominami,pesos) |>
  count(huerfano_pol,opinion_ominami,wt = pesos) |>
  filter(!is.na(opinion_ominami)) |>
  filter(!is.na(huerfano_pol)) |>
  filter(huerfano_pol!="-") |>
  group_by(huerfano_pol) |>
  mutate(media = n /sum(n)) |>
  mutate(aspecto = "opinion_ominami" ) |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  rename(respuesta = "opinion_ominami" ) |>
  #filter(huerfano_pol %in% c("Michelle Bachelet","Tomás Vodanovic")) |>
  mutate(tema = paste0(huerfano_pol))



p_opinion_ominami_huerfano_pol_graf<-
  bd_opinion_ominami_huerfano_pol |>
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
    caption_opinion  ="Opinion de Marco Enríquez-Ominami, por huerfano_pol" ,
    size_caption_opinion = 12,

    #CONOCIMIENTO
    burbuja = bd_conoce_ominami_huerfano_pol |> filter(respuesta == 'Sí') |> rename(valor =  respuesta),
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













#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
##### Pres
#*#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
library(officer)
path_export <-
  encuestar:::formato_archivo(nombre = "./data-raw/press/cruces_publico_objetivo",
                              extension = "pptx",
                              tolerancia = 60)

dia <- lubridate::today() |> lubridate::day()




pptx <-
  read_pptx(path = "./data-raw/plantilla_general_09_12_24.pptx")



add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Bloque de voto por Michelle Bachelet',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = sexo_rango_edad_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Distribución de sexo y edad general',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
  ph_with(value = p_cualidades_valora_candidato_voto_pr_bachelet_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Cualidades de un candidato del electorado de Michelle Bachelet',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
  ph_with(value = p_problematicas_voto_pr_bachelet_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Principales problemáticas en Chile del electorado de Michelle Bachelet',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_medios_com_voto_pr_bachelet, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Principales medios de comunicación para el electorado de Michelle Bachelet',
          location = ph_location_label(ph_label = "titulo"))



# mUJERES

add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Bloque Mujeres',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
  ph_with(value = p_cualidades_valora_candidato_mujeres_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Cualidades de un candidato para las mujeres',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
  ph_with(value = p_problematicas_mujeres_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Principales problemáticas en Chile para las mujeres',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_medios_com_mujeres, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Principales medios de comunicación para las mujeres',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_opinion_ominami_sexo_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Opinion de Ominami por sexo',
          location = ph_location_label(ph_label = "titulo"))


# huerfanos politicos

add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Bloque Huérfanos políticos',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = huerfanos_pol_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Proporcion de huérfanos políticos',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
  ph_with(value = p_cualidades_valora_candidato_huerfanos_politicos_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Cualidades de un candidato para los huérfanos políticos',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
  ph_with(value = p_problematicas_huerfanos_politicos_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Principales problemáticas en Chile para los huérfanos políticos',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_medios_com_huerfano_pol, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Principales medios de comunicación para los huérfanos políticos',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_opinion_ominami_huerfano_pol_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Opinion de Ominami para huérfanos políticos',
          location = ph_location_label(ph_label = "titulo"))

print(pptx, path_export)
