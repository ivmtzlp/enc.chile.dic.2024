
# Bloque Contexto Social ----------------------------------------------------------------------

# pREGUNTA RANDOM
p_temas_tit <-
  diccionario |>
  filter(llave == 'temas') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_temas <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(temas) |>
  asignar_colores()

# medios de comunicacion
p_medios_com_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(llave == "medios_com") |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)

colores_medios_com <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(medios_com) |>
  asignar_colores()

# Redes sociales que utiliza
p_utiliza_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "utiliza", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)

colores_utiliza <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "utiliza", x = llave)) |>
  distinct(tema) |>
  asignar_colores()

# PROBLEMA
p_problema_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "problema", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)

colores_problema <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("problema")) |>
  select(contains("_O")) |>
  tidyr::pivot_longer(cols = everything(),
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  na.omit() |>
  distinct(respuesta) |>
  asignar_colores()

# Calificacion Gobierno
p_calificacion_gobierno <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "cali", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)


# Chile Actual
p_chile_actual_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "chile_actual", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)

colores_chile_actual <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(chile_actual) |>
  asignar_colores()

# Chile futuro
p_chile_futuro_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "chile_futuro", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_chile_futuro <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(chile_futuro) |>
  asignar_colores()

# Frases ricos
p_frases_ricos_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "frases_ricos", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_frases_ricos <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(frases_ricos) |>
  asignar_colores()

# Frases gobierno
p_frases_gobierno_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "frases_gobierno", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_frases_gobierno <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(frases_gobierno) |>
  asignar_colores()

# Satisfaccion democracia
p_satisfaccion_democracia_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "satisfaccion_democracia", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_satisfaccion_democracia <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(satisfaccion_democracia) |>
  asignar_colores()

# Derecha izquierda
p_izquierda_derecha_tit <-
  diccionario |>
  filter(bloque == "Contexto social") |>
  filter(grepl(pattern = "escala", x = llave)) |>
  distinct(pregunta) |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)
