
bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_primarias)|>
  naniar::vis_miss()
#Constantes   ########################################33

# Interes Politica
orden_interes_politica <- c("Muy interesado",   "Interesado",   "Neutral/Indiferente",   "Nada interesado", "Muy poco interesado", "Ns/Nc" )

p_interes_politica_tit <-
  diccionario |>
  filter(grepl('interes_politica',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

# Interes eleccion municipal
p_interes_eleccion_mun_24_tit <-
  diccionario |>
  filter(grepl('interes_eleccion_mun_24',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()


# Perticipacion presidencia 21
p_participacion_pr_21_tit <-
  diccionario |>
  filter(grepl('participacion_pr_21',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()



# Perticipacion municipal 24
p_participacion_mun_24_tit <-
  diccionario |>
  filter(grepl('participacion_mun_24',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()


#Voto proximas elecciones
p_voto_proximas_elecciones_tit <-
  diccionario |>
  filter(grepl('voto_proximas_elecciones',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

# Perticipacion primarias 25
p_participacion_primarias_tit <-
  diccionario |>
  filter(grepl('participacion_primarias',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()



# Opinion primarias
p_opinion_primarias_tit <-
  diccionario |>
  filter(grepl('opinion_primarias',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()


# Voto Pr 25
p_voto_pr_tit <-
  diccionario |>
  filter(grepl('voto_pr',llave)) |>
  filter(!grepl('proximas',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()


# Voto segundo Pr 25
p_voto2_pr_tit <-
  diccionario |>
  filter(grepl('voto2_pr',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()



# candidato nunca voto pr 25
p_candidato_nunca_voto_tit <-
  diccionario |>
  filter(grepl('candidato_nunca_voto',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()


# postura ideologica
p_definicion_postura_ideologica_tit <-
  diccionario |>
  filter(grepl('definicion_postura_ideologica',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

orden_definicion_postura_ideologica <- c("Izquierda","Centro izquierda","Centro","Centro derecha","Derecha","Ninguno",
                                         "Ns/Nc" )


# Identificacion partidista
p_identificacion_partido_tit <-
  diccionario |>
  filter(grepl('identificacion_partido',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()
#######################################333

# Interes Politica
bd_interes_politica<-
bd_respuestas_efectivas |>
  as_tibble() |>
  select(interes_politica) |>
  count(interes_politica) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=interes_politica )


p_interes_politica_graf<-
bd_interes_politica|>
  graficar_barras(orden_respuestas = rev(orden_interes_politica))+
  labs(caption = p_interes_politica_tit)+
  tema_morant()



# Interes eleccion municipal
bd_interes_eleccion_mun_24<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(interes_eleccion_mun_24) |>
  count(interes_eleccion_mun_24) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=interes_eleccion_mun_24 )


p_interes_eleccion_mun_24_graf<-
  bd_interes_eleccion_mun_24|>
  graficar_barras(orden_respuestas = rev(orden_interes_politica))+
  labs(caption = p_interes_eleccion_mun_24_tit)+
  tema_morant()



# Perticipacion presidencia 21
bd_participacion_pr_21<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_pr_21) |>
  count(participacion_pr_21) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_pr_21 ) |>
  filter(respuesta == 'Sí')


p_participacion_pr_21_graf <-
bd_participacion_pr_21 |>
  graficar_gauge(color_principal = "green",escala = c(0,1),size_text_pct = 25)+
  labs(caption = p_participacion_pr_21_tit)


# Perticipacion municipal 24
bd_participacion_mun_24<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_mun_24) |>
  count(participacion_mun_24) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_mun_24 ) |>
  filter(respuesta == 'Sí')


p_participacion_mun_24_graf <-
  bd_participacion_mun_24 |>
  graficar_gauge(color_principal = "green",escala = c(0,1),size_text_pct = 25)+
  labs(caption = p_participacion_mun_24_tit)



#Voto proximas elecciones
bd_voto_proximas_elecciones<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_proximas_elecciones) |>
  count(voto_proximas_elecciones) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_proximas_elecciones)


p_voto_proximas_elecciones_graf<-
  bd_voto_proximas_elecciones|>
  graficar_barras()+
  #graficar_barras(orden_respuestas = rev(orden_voto_proximas_elecciones))+
  labs(caption = p_voto_proximas_elecciones_tit)+
  tema_morant()


# Perticipacion primarias 25
bd_participacion_primarias<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_primarias) |>
  count(participacion_primarias) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_primarias ) |>
  filter(respuesta == 'Sí')


p_participacion_primarias_graf <-
  bd_participacion_primarias |>
  graficar_gauge(color_principal = "green",escala = c(0,1),size_text_pct = 25)+
  labs(caption = p_participacion_primarias_tit)




# Voto Pr 25
bd_voto_pr<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr) |>
  filter(!is.na(voto_pr)) |>
  count(voto_pr) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_pr)


p_voto_pr_graf<-
  bd_voto_pr|>
  graficar_barras()+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  labs(caption = p_voto_pr_tit)+
  tema_morant()


# Voto segundo Pr 25
bd_voto2_pr<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto2_pr) |>
  filter(!is.na(voto2_pr)) |>
  count(voto2_pr) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto2_pr)


p_voto2_pr_graf<-
  bd_voto2_pr|>
  graficar_barras()+
  #graficar_barras(orden_respuestas = rev(orden_voto2_pr))+
  labs(caption = p_voto2_pr_tit)+
  tema_morant()



# candidato nunca voto pr 25
bd_candidato_nunca_voto<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(candidato_nunca_voto) |>
  filter(!is.na(candidato_nunca_voto)) |>
  count(candidato_nunca_voto) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=candidato_nunca_voto)


p_candidato_nunca_voto_graf<-
  bd_candidato_nunca_voto|>
  graficar_barras()+
  #graficar_barras(orden_respuestas = rev(orden_candidato_nunca_voto))+
  labs(caption = p_candidato_nunca_voto_tit)+
  tema_morant()


# postura ideologica
bd_definicion_postura_ideologica<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(definicion_postura_ideologica) |>
  filter(!is.na(definicion_postura_ideologica)) |>
  count(definicion_postura_ideologica) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=definicion_postura_ideologica)



p_definicion_postura_ideologica_graf<-
  bd_definicion_postura_ideologica|>
  #graficar_barras()+
  graficar_barras(orden_respuestas = rev(orden_definicion_postura_ideologica))+
  labs(caption = p_definicion_postura_ideologica_tit)+
  tema_morant()




# Identificacion partidista
bd_identificacion_partido<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(identificacion_partido) |>
  #filter(!is.na(identificacion_partido)) |>
  count(identificacion_partido) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=identificacion_partido)


p_identificacion_partido_graf<-
  bd_identificacion_partido|>
  graficar_barras()+
  #graficar_barras(orden_respuestas = rev(orden_identificacion_partido))+
  labs(caption = p_identificacion_partido_tit)+
  tema_morant()


















