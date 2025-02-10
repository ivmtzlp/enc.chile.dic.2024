
# bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(participacion_primarias)|>
#   naniar::vis_miss()

source('./data-raw/scripts/parametros/parametros_bloque_participacion_politica.R')

#######################################333



# Interes eleccion municipal
bd_interes_eleccion_mun_24<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(interes_eleccion_mun_24,pesos) |>
  count(interes_eleccion_mun_24,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=interes_eleccion_mun_24 )


p_interes_eleccion_mun_24_graf <-
  bd_interes_eleccion_mun_24|>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(orden_interes_politica))+

  labs(caption = p_interes_eleccion_mun_24_tit)+
  scale_fill_manual(values = colores_interes_eleccion_mun_24) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



# Perticipacion presidencia 21
bd_participacion_pr_21<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_pr_21,pesos) |>
  count(participacion_pr_21,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_pr_21 ) |>
  filter(respuesta == 'Sí')


p_participacion_pr_21_graf <-
bd_participacion_pr_21 |>
  graficar_gauge(color_principal = color_general,escala = c(0,1),size_text_pct = 12)+
  labs(title = paste0(p_participacion_pr_21_tit,
       '\nEntrevistados que respondieron "Sí"'))+
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(size = 12))


# Perticipacion municipal 24
bd_participacion_mun_24<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_mun_24,pesos) |>
  count(participacion_mun_24,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_mun_24 ) |>
  filter(respuesta == 'Sí')


p_participacion_mun_24_graf <-
  bd_participacion_mun_24 |>
  graficar_gauge(color_principal =color_general,escala = c(0,1),size_text_pct = 12)+
  labs(title = paste0(p_participacion_mun_24_tit,
                      '\nEntrevistados que respondieron "Sí"'))+
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(size = 12))



#Voto proximas elecciones
bd_voto_proximas_elecciones<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_proximas_elecciones,pesos) |>
  count(voto_proximas_elecciones,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_proximas_elecciones)


p_voto_proximas_elecciones_graf<-
  bd_voto_proximas_elecciones|>
  graficar_barras(salto = 20,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.05)+
  #graficar_barras(orden_respuestas = rev(orden_voto_proximas_elecciones))+
  scale_fill_manual(values = colores_voto_proximas_elecciones) +
  labs(caption = p_voto_proximas_elecciones_tit)+
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 13),
        plot.caption = element_text(size = 12))


# Perticipacion primarias 25
bd_participacion_primarias<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_primarias,pesos) |>
  count(participacion_primarias,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_primarias ) |>
  filter(respuesta == 'Sí')


p_participacion_primarias_graf <-
  bd_participacion_primarias |>
  graficar_gauge(color_principal = color_general,escala = c(0,1),size_text_pct = 12)+
  labs(title = paste0(p_participacion_primarias_tit,
                      '\nEntrevistados que respondieron "Sí"'))+
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(size = 12))


# Voto Pr 25
bd_voto_pr<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr,pesos) |>
  filter(!is.na(voto_pr)) |>
  count(voto_pr,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_pr)


p_voto_pr_graf<-
  bd_voto_pr|>
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
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


# Voto segundo Pr 25
bd_voto2_pr<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto2_pr,pesos) |>
  filter(!is.na(voto2_pr)) |>
  count(voto2_pr,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto2_pr)


p_voto2_pr_graf<-
  bd_voto2_pr|>
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
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



# candidato nunca voto pr 25
bd_candidato_nunca_voto<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(candidato_nunca_voto,pesos) |>
  filter(!is.na(candidato_nunca_voto)) |>
  count(candidato_nunca_voto,wt=pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=candidato_nunca_voto)


p_candidato_nunca_voto_graf<-
  bd_candidato_nunca_voto|>
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
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


# postura ideologica
bd_definicion_postura_ideologica<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(definicion_postura_ideologica,pesos) |>
  #filter(!is.na(definicion_postura_ideologica)) |>
  count(definicion_postura_ideologica,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=definicion_postura_ideologica)



p_definicion_postura_ideologica_graf<-
  bd_definicion_postura_ideologica|>
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




# Identificacion partidista
bd_identificacion_partido<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(identificacion_partido,pesos) |>
  #filter(!is.na(identificacion_partido)) |>
  count(identificacion_partido,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=identificacion_partido)


p_identificacion_partido_graf<-
  bd_identificacion_partido|>
  graficar_barras(salto = 40,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.03)+
  #graficar_barras(orden_respuestas = rev(orden_identificacion_partido))+
  scale_y_continuous(limits = c(0, 0.75),
                     labels = scales::percent) +
  scale_fill_manual(values = colores_identificacion_partido) +
  labs(caption = p_identificacion_partido_tit)+
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))


############################################################################################
############################################################################################
############################################################################################
#Cruces
############################################################################################
############################################################################################
############################################################################################
principales_cand <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr,pesos) |>
  filter(!is.na(voto_pr)) |>
  count(voto_pr,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  filter(!voto_pr %in% c("Ninguno","Ns/Nc") ) |>
  mutate(rango = dense_rank(x=-media) ) |>
  filter(rango <=4 | voto_pr == 'Marco Enríquez-Ominami') |>
  arrange(rango) |>
  pull(voto_pr)

#####################
# Voto Pr por sexo 25
#####################
bd_sexo_voto_pr<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr,sexo,pesos) |>
  filter(!is.na(voto_pr)) |>
  filter(!is.na(sexo)) |>
  count(voto_pr,sexo,wt = pesos) |>
  group_by(voto_pr) |>
  mutate(media = n /sum(n)) |>
  rename(tema=voto_pr,variable_principal = sexo, mean=media) |>
  ungroup()


# Invertir variables
bd_sexo_voto_pr <-
  bd_sexo_voto_pr |>
  transmute(aux = variable_principal,
            variable_principal = tema,
            tema = aux,
            mean) |>
  select(!aux)

# Ver diferencias
bd_sexo_voto_pr <-
  bd_sexo_voto_pr |>
  group_by(variable_principal)|>
  mutate(mean_diff_pos = min(mean) + (max(mean)-min(mean))/2,
         mean_dif = (max(mean)-min(mean)))|>
  ungroup()



voto_pr_sexo_graf <-
  bd_sexo_voto_pr |>
  mutate(cv= 0) |>
  filter(variable_principal %in% principales_cand) |>
  graficar_crucePuntos(cruce = 'variable_principal',
                       vartype = 'cv',
                       variable = 'tema',
                       size_pct = 5,
                       orden_cruce = rev(principales_cand),traslape = T,limite_dif_pct = 0.3,ajuste_pos = 0.015)+
  scale_color_manual(values = c('Mujer'= color_m,"Hombre"=color_h))+
  #scale_x_discrete(labels= c('F'='Mujer','M'='Hombre'))+
  scale_y_continuous(limits = c(0, .75),
                     labels = scales::percent)+
  labs(caption =p_voto_pr_tit)+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

#####################
# Voto Pr por generacion 25
#####################
bd_generacion_voto_pr<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr,generacion,pesos) |>
  filter(!is.na(voto_pr)) |>
  filter(!is.na(generacion)) |>
  count(voto_pr,generacion,wt = pesos) |>
  group_by(voto_pr) |>
  mutate(media = n /sum(n)) |>
  rename(tema=voto_pr,variable_principal = generacion, mean=media) |>
  ungroup()


# Invertir variables
bd_generacion_voto_pr <-
  bd_generacion_voto_pr |>
  transmute(aux = variable_principal,
            variable_principal = tema,
            tema = aux,
            mean) |>
  select(!aux)

# Ver diferencias
bd_generacion_voto_pr <-
  bd_generacion_voto_pr |>
  group_by(variable_principal)|>
  mutate(mean_diff_pos = min(mean) + (max(mean)-min(mean))/2,
         mean_dif = (max(mean)-min(mean)))|>
  ungroup()


voto_pr_generacion_graf <-
  bd_generacion_voto_pr |>
  filter(variable_principal %in% principales_cand) |>
  graficar_lolipop_diferencias(orden_variablePrincipal = rev(principales_cand),
                               colores_variables_secundarias = colores_generacion,
                               nudge_x = 0.25,limits = c(0,.6),
                               traslape = T,
                               limite_dif_pct = 0.03,
                               ajuste_pos = 0.007)+
  labs(caption = p_voto_pr_graf)+
  tema_morant() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        plot.caption = element_text(size = 12))+
  guides(color = guide_legend(ncol  = 2))



#####################
# Temas interes politica
#####################

# bd_temas_interes_politica <-
# bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(temas,interes_politica,pesos) |>
#   filter(!is.na(temas)) |>
#   filter(!is.na(interes_politica)) |>
#   count(temas,interes_politica,wt = pesos) |>
#   group_by(temas) |>
#   mutate(media = n /sum(n)) |>
#   ungroup() |>
#   tidyr::complete(temas,interes_politica,fill = list(n = 0, media =0)) |>
#
#   mutate(media = scales::percent(x = media,accuracy=1.0))
#
#
# orden_temas_interes_politica <-
# bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(temas,pesos) |>
#   count(temas,wt = pesos) |>
#   mutate(media = n/sum(n)) |>
#   arrange(desc(media)) |>
#   mutate(temas = as.character(temas)) |>
#   select(temas, media)
#
# colores_temas_interes_2 <-
# orden_temas_interes_politica |>
#   select(temas) |>
#   asignar_colores()
#
#
# colores_temas_interes_2["Política"] <- color_ominami
# colores_temas_interes_2<- colores_temas_interes_2[!names(colores_temas_interes_2) %in% c("Salud","Deportes", "Películas")]
#
#
#
# bd_temas_interes_politica <-
# bd_temas_interes_politica|>
#   mutate(interes_politica = factor(interes_politica,
#                                    levels = c("Muy interesado","Interesado" ,"Neutral/Indiferente","Muy poco interesado","Nada interesado","Ns/Nc")  )) |>
#   arrange(interes_politica) |>
#   tidyr::pivot_wider(id_cols = temas,
#                      names_from = interes_politica,
#                      values_from = media) |>
#  left_join(orden_temas_interes_politica,
#            by = "temas") |>
#   arrange(desc(media)) |>
#   select(!media) |>
#   rename(respuesta = temas)
#
#
# temas_interes_politica_tbl <-
# bd_temas_interes_politica|>
# encuestar:::formatear_tabla_votoCruzado(
#                             var1 = "respuesta",
#                             var2 = "",
#                             filtro_var2 = NULL,
#                             etiquetas = c("Temas de interés","Nivel de interés /nen la política"),
#                             colores_var1 = colores_temas_interes_2,
#                             colores_var2 = rep("white",7),
#                             size_text_header = 18,
#                             size_text_body = 14,
#                             salto = 20
#                             )|>
#   flextable::color(color = "black", part = "header", i = 2) |>
#   flextable::bg(i = ~ respuesta == 'Política', bg=color_ominami,part="body") |>
#   flextable::color(i = ~ respuesta == 'Política', color='white',part="body")
#


#####################
# Temas interes politica
#####################








#####################
# Temas interes politica
#####################
voto_pr_voto2_pr_ca_graf <-
encuestar:::analisis_correspondencia(var1 = "voto_pr",
                                     var2 = "voto2_pr",
                                     legenda1 = "Primera opción \nde voto",
                                     legenda2 = "Segunda opción \nde voto",
                                     diseno = calibrated_design
                                     )+
  tema_transparente()


voto_pr_voto2_pr_bd <-
bd_respuestas_efectivas |>
  count(voto_pr,voto2_pr,wt = pesos) |>
  group_by(voto_pr) |>
  mutate(media = n/sum(n)) |>
  filter(voto_pr %in%   (bd_voto_pr |> top_n(n = 3,wt = media) |> pull(respuesta))) |>
  rename(respuesta=voto2_pr)


voto_pr_voto2_pr_graf <-
voto_pr_voto2_pr_bd |>
  graficar_barras() +
  scale_fill_manual(values = colores_voto_pr ) +
  labs(caption = p_voto2_pr_tit)+
  facet_wrap(facets = ~voto_pr)+
  tema_morant()

voto_pr_voto2_pr_v2_bd <-
  bd_respuestas_efectivas |>
  count(voto_pr,voto2_pr,wt = pesos) |>
  group_by(voto_pr) |>
  mutate(media = n/sum(n)) |>
  filter(voto_pr %in%   c("Tomás Vodanovic","Marco Enríquez-Ominami")) |>
  rename(respuesta=voto2_pr)

voto_pr_voto2_pr_v2_graf <-
  voto_pr_voto2_pr_v2_bd |>
  graficar_barras() +
  scale_fill_manual(values = colores_voto_pr ) +
  labs(caption = p_voto2_pr_tit)+
  facet_wrap(facets = ~voto_pr)+
  tema_morant()

voto_pr_candidato_nunca_voto_ca_graf <-
  encuestar:::analisis_correspondencia(var1 = "voto_pr",
                                       var2 = "candidato_nunca_voto",
                                       legenda1 = "Primera opción \nde voto",
                                       legenda2 = "Candidato por el que\nnunca votaría",
                                       diseno = calibrated_design
  )+
  tema_transparente()
###################################
#--------------------interes politica
##################################


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
  encuestar:::analisis_correspondencia(var1 = "interes_politica",
                                      var2 = "voto_pr",
                                      legenda1 = "Interés en la política",
                                      legenda2 = "Preferencia de voto",
                                      diseno = calibrated_design)+
  tema_transparente()






################################################
# voto pr perfil
################################################
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
                  text_size = 5,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.07)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, .75),
                     labels = scales::percent) +
  labs(caption = paste0("Voto a la presidencia de Chile","\nTres principales perfiles de inclinación al voto"))+
  tema_morant()+
  facet_wrap(~voto_proximas_elecciones)+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


