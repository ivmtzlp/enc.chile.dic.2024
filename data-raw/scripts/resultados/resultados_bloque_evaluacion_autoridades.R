

source(file = './data-raw/scripts/parametros/parametros_bloque_evaluacion_autoridades.R')

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
    regular = "No aprueba ni desaprueba",
    orden_resp = rev(c("Desaprueba mucho","Desaprueba poco","No aprueba ni desaprueba",
                       "Aprueba poco","Aprueba mucho")),
    grupo_positivo = c("Aprueba poco","Aprueba mucho"),
    grupo_negativo = rev(c("Desaprueba mucho","Desaprueba poco")),
    caption_opinion  = aprueba_autoridades_tit,
    size_caption_opinion = 12,
    burbuja = NULL,


    #NO SABE NO CONTESTA
    ns_nc ="Ns/Nc",
    caption_nsnc = "Ns/Nc",
    size_caption_nsnc = 12,
    color_nsnc = "gray50"

  )




