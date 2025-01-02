#diccionario |> View()
# library(tidyverse)


source('./data-raw/scripts/parametros/parametros_bloque_conocimiento_personajes.R')
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
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.05)+
  scale_fill_manual(values = colores_conoce_per)+
  labs(caption = p_conoce_per_tit  )+
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



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

# calif_winter

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
  graficar_intervalo_numerica(escala = c(1,7),text_point_size = 5,nudge_x = 0.4 ) +
  labs(caption = p_calif_per_tit) +
  scale_y_binned(labels = c(1:7),limits = c(1,7))+
  tema_morant()


