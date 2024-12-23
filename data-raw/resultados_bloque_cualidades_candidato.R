# bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(participacion_primarias)|>
#   naniar::vis_miss()
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
  tidyr::pivot_longer(cols = everything(),
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  na.omit() |>
  distinct(respuesta) |>
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
  tidyr::pivot_longer(cols = everything(),
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  na.omit() |>
  distinct(respuesta) |>
  asignar_colores()


# Aprobacion auntoridades

aprueba_autoridades_vars <- c('aprueba_gobierno_boric','aprueba_ministros')
#######################################333

# Cualidad mas valorada

bd_cualidades_valora_candidato <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(SbjNum, contains("cualidades_valora_candidato_O")) |>
  tidyr::pivot_longer(cols = !SbjNum,
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  na.omit() |>
  count(respuesta) |>
  mutate(pct = n/nrow(bd_respuestas_efectivas))

p_cualidades_valora_candidato_graf <-
  bd_cualidades_valora_candidato |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_cualidades_valora_candidato) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_cualidades_valora_candidato_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))

# Nesecidad economica chile
bd_necesita_chile_economia<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(necesita_chile_economia) |>
  count(necesita_chile_economia) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=necesita_chile_economia )

p_necesita_chile_economia_graf<-
  bd_necesita_chile_economia|>
  #graficar_barras(orden_respuestas = rev(orden_necesita_chile_economia))+
  graficar_barras()+
  scale_fill_manual(values = colores_necesita_chile_economia) +
  labs(caption = p_necesita_chile_economia_tit)+
  tema_morant()+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))


# Necesidad consesnso chile
bd_necesita_chile_consenso <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(necesita_chile_consenso) |>
  count(necesita_chile_consenso) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=necesita_chile_consenso )

p_necesita_chile_consenso_graf <-
  bd_necesita_chile_consenso|>
  #graficar_barras(orden_respuestas = rev(orden_necesita_chile_consenso))+
  graficar_barras()+
  scale_fill_manual(values = colores_necesita_chile_consenso) +
  labs(caption = p_necesita_chile_consenso_tit)+
  tema_morant()+
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))




bd_necesita_chile_consenso <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(aprueba_gobierno_boric) |>
  count(aprueba_gobierno_boric) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=necesita_chile_consenso )

# Aprobacion auntoridades

bd_aprueba_autoridades <-
  aprueba_autoridades_vars |>
  purrr::map_df(.f = ~{


    bd_respuestas_efectivas |>
      select(all_of(.x)) |>
      count(!!rlang::sym(.x)) |>
      filter(!is.na(!!rlang::sym(.x))) |>
      mutate(media = n /sum(n)) |>
      mutate(aspecto = .x ) |>
      left_join(diccionario |>
                  select(llave,tema),
                by = c('aspecto' = 'llave' )) |>
      rename('respuesta' := .x )
  })


p_aprueba_autoridades_graf<-
  bd_aprueba_autoridades |>
  graficar_candidato_opinion(

    #patron_inicial = "opinion",
    #aspectos = aspectos_conoce_per,
    size_text_cat = 16,

    #OPINION
    salto = 45,
    colores = colores_aprueba_autoridades,
    regular = "",
    orden_resp = c("Negativa","Positiva"),
    grupo_positivo = c("Positiva"),
    grupo_negativo = rev(c("Negativa")),
    caption_opinion  =p_aprueba_autoridades_tit ,
    size_caption_opinion = 12,

    #CONOCIMIENTO
    burbuja = bd_conoce_per |> filter(respuesta == 'Sí') |> rename(valor =  respuesta),
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










