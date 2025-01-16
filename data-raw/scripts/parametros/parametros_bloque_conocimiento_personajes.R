
# Conocimiento personajes
conoce_per_vars<- diccionario |>
  filter(grepl('conoce_per',llave)) |>
  select(llave) |>
  pull()

p_conoce_per_tit <- diccionario |>
  filter(grepl('conoce_per',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()


aspectos_conoce_per <-  gsub(pattern = 'conoce_per_',replacement = '',conoce_per_vars)

colores_conoce_per <-  c("Michelle Bachelet" = color_general,
                         "Evelyn Mathei" = color_general,
                         "José Antonio Kast" = color_general,
                         "Carolina Tohá" = color_general,
                         "Marco Enríquez-Ominami" = color_ominami,
                         "Franco Parisi" = color_general,
                         "Tomás Vodanovic" = color_general,
                         "Johannes Kaiser" = color_general,
                         "Gonzalo Winter" = color_general)

# Opinion personajes

opinion_per_vars<- diccionario |>
  filter(grepl('opinion_',llave)) |>
  filter(!grepl('razon_',llave)) |>
  filter(!grepl('_primarias',llave)) |>
  select(llave) |>
  pull()

p_opinion_per_tit <- diccionario |>
  filter(grepl('opinion_',llave)) |>
  filter(!grepl('razon_',llave)) |>
  filter(!grepl('_primarias',llave)) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()


colores_opinion_per <-
  c("Negativa" = color_opinion_muyMala,
    #"Mala" = color_opinion_mala,
    #"Regular" = color_opinion_regular,
    # "Buena" = color_opinion_buena,
    "Positiva" = color_opinion_muyBuena)

#aspectos_opinion_per <-  gsub(pattern = 'opinion_',replacement = '',opinion_per_vars)


#  Opinion personajes
calif_per_vars <- paste0('calif_',aspectos_conoce_per)

p_calif_per_tit <- diccionario |>
  filter(llave  %in% calif_per_vars) |>
  select(pregunta) |>
  distinct(pregunta) |>
  pull()
