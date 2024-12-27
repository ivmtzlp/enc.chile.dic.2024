

#Constantes   ########################################33

# Cualidad mas valorada
orden_cualidades_valora_candidato <- c("Muy interesado",   "Interesado",   "Neutral/Indiferente",   "Nada interesado", "Muy poco interesado", "Ns/Nc" )

p_cualidades_valora_candidato_tit <-
  diccionario |>
  filter(grepl('cualidades_valora_candidato',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

colores_cualidades_valora_candidato <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("cualidades_valora_candidato_O")) |>
  select(contains("_O")) |>
  tidyr::pivot_longer(cols = everything(),
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  na.omit() |>
  distinct(respuesta) |>
  asignar_colores()


# Nesecidad economica chile
#orden_cualidades_valora_candidato <- c("Muy interesado",   "Interesado",   "Neutral/Indiferente",   "Nada interesado", "Muy poco interesado", "Ns/Nc" )

p_necesita_chile_economia_tit <-
  diccionario |>
  filter(grepl('necesita_chile_economia',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

colores_necesita_chile_economia <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("necesita_chile_economia")) |>
  na.omit() |>
  distinct(necesita_chile_economia) |>
  asignar_colores()


# Nesecidad consesnso chile
p_necesita_chile_consenso_tit <-
  diccionario |>
  filter(grepl('necesita_chile_consenso',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()

colores_necesita_chile_consenso <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("necesita_chile_consenso")) |>
  distinct(necesita_chile_consenso) |>
  asignar_colores()


# Aprobacion auntoridades

aprueba_autoridades_vars <- c('aprueba_gobierno_boric','aprueba_ministros')

colores_aprueba_autoridades <-c("Desaprueba mucho"= color_opinion_muyMala,
                                "Desaprueba poco"= color_opinion_mala,
                                "No aprueba ni desaprueba"= color_opinion_regular,
                                "Aprueba poco"= color_opinion_buena,
                                "Aprueba mucho"= color_opinion_muyBuena)


