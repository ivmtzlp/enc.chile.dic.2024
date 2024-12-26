#Constantes   ########################################33

# Interes Politica
orden_interes_politica <- c("Muy interesado",   "Interesado",   "Neutral/Indiferente",   "Nada interesado", "Muy poco interesado", "Ns/Nc" )

p_interes_politica_tit <-
  diccionario |>
  filter(grepl('interes_politica',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

colores_interes_politica <-
  c("Muy interesado" = color_general,
    "Interesado" = color_general,
    "Neutral/Indiferente" = color_general,
    "Muy poco interesado" = color_general,
    "Nada interesado" = color_general,
    "Ns/Nc" = color_nsnc)

# Interes eleccion municipal
p_interes_eleccion_mun_24_tit <-
  diccionario |>
  filter(grepl('interes_eleccion_mun_24',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

colores_interes_eleccion_mun_24 <-
c("Muy interesado" = color_general,
  "Interesado" = color_general,
  "Neutral/Indiferente" = color_general,
  "Muy poco interesado" = color_general,
  "Nada interesado" = color_general,
  "Ns/Nc" = color_nsnc)

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

colores_voto_proximas_elecciones <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("voto_proximas_elecciones")) |>
  na.omit() |>
  distinct(voto_proximas_elecciones) |>
  asignar_colores()

# Perticipacion primarias 25
p_participacion_primarias_tit <-
  diccionario |>
  filter(grepl('participacion_primarias',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull() |>
  stringr::str_wrap(width = 40)



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

colores_voto_pr <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("voto_pr")) |>
  select(-contains('otro')) |>
  na.omit() |>
  distinct(voto_pr) |>
  asignar_colores()

colores_voto_pr["Marco Enríquez-Ominami"] <- color_ominami


# Voto segundo Pr 25
p_voto2_pr_tit <-
  diccionario |>
  filter(grepl('voto2_pr',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

colores_voto2_pr <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("voto2_pr")) |>
  select(-contains('otro')) |>
  na.omit() |>
  distinct(voto2_pr) |>
  asignar_colores()

colores_voto2_pr["Marco Enríquez-Ominami"] <- color_ominami

# candidato nunca voto pr 25
p_candidato_nunca_voto_tit <-
  diccionario |>
  filter(grepl('candidato_nunca_voto',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()


colores_candidato_nunca_voto <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("candidato_nunca_voto")) |>
  select(-contains('otro')) |>
  na.omit() |>
  distinct(candidato_nunca_voto) |>
  asignar_colores()

colores_candidato_nunca_voto["Marco Enríquez-Ominami"] <- color_ominami

# postura ideologica
p_definicion_postura_ideologica_tit <-
  diccionario |>
  filter(grepl('definicion_postura_ideologica',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

orden_definicion_postura_ideologica <- c("Izquierda","Centro izquierda","Centro","Centro derecha","Derecha","Ninguno",
                                         "Ns/Nc" )


colores_definicion_postura_ideologica <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("definicion_postura_ideologica")) |>
  select(-contains('otro')) |>
  na.omit() |>
  distinct(definicion_postura_ideologica) |>
  asignar_colores()

# Identificacion partidista
p_identificacion_partido_tit <-
  diccionario |>
  filter(grepl('identificacion_partido',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()



colores_identificacion_partido <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("identificacion_partido")) |>
  select(-contains('otro')) |>
  na.omit() |>
  distinct(identificacion_partido) |>
  asignar_colores()
