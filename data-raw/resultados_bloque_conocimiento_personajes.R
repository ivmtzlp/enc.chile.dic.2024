#diccionario |> View()
library(tidyverse)


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

################################################################################




# Conocimiento personajes
bd_conoce_per <-
conoce_per_vars |>
  purrr::map_df(.f = ~{


    bd_respuestas_efectivas |>
      select(all_of(.x)) |>
      count(!!rlang::sym(.x)) |>
      mutate(media = n /sum(n)) |>
      mutate(aspecto = .x ) |>
      left_join(diccionario |>
                  select(llave,tema),
                by = c('aspecto' = 'llave' )) |>
      rename('respuesta' := .x )
    })


p_conoce_per_graf <-
bd_conoce_per |>
  filter(respuesta == 'Sí') |>
  rename(valor =  respuesta,
         respuesta = tema) |>
  graficar_barras()+
  labs(caption = p_conoce_per_tit  )+
  tema_morant()



# Opinion personajes

bd_opinion_per <-
  opinion_per_vars |>
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


p_opinion_per_graf<-
bd_opinion_per |>
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
    caption_opinion  =p_opinion_per_tit ,
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


# Calif personajes
#calif_per_vars |>

calif_winter

lista_calif_per <-
calif_per_vars   |>
purrr::map(.f= ~{
  bd_respuestas_efectivas |>
    calcular_resultados_calificacion(variable = .x) |>
    append(list(aspecto = .x  ))
})


bd_calif_per <- do.call(rbind, lapply(lista_calif_per, as.data.frame))

bd_calif_per <-
bd_calif_per |>
  left_join(diccionario |>
              select(llave,tema),
            by = c('aspecto' = 'llave' )) |>
  mutate(inf = media,
         sup =  media
         )

p_calif_per_graf <-
bd_calif_per |>
  graficar_intervalo_numerica(escala = c(1,7),text_point_size = 6) +
  labs(caption = p_calif_per_tit) +
  scale_y_binned(labels = c(1:7),limits = c(1,7))+
  tema_morant()


