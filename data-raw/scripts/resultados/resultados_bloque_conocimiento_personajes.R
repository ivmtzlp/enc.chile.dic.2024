#diccionario |> View()
# library(tidyverse)


source('./data-raw/scripts/parametros/parametros_bloque_conocimiento_personajes.R')
################################################################################




# Conocimiento personajes
bd_conoce_per <-
conoce_per_vars |>
  purrr::map_df(.f = ~{


    bd_respuestas_efectivas |>
      select(all_of(.x),pesos) |>
      count(!!rlang::sym(.x),wt = pesos) |>
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
      select(all_of(.x),pesos) |>
      count(!!rlang::sym(.x),wt = pesos) |>
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


##### Conocimiento vs sexo
bd_conoce_per_sexo <-
  conoce_per_vars |>
  purrr::map_df(.f = ~{


    bd_respuestas_efectivas |>
      select(all_of(.x),sexo,pesos) |>
      count(!!rlang::sym(.x),sexo,wt = pesos) |>
      mutate(media = n /sum(n)) |>
      mutate(aspecto = .x ) |>
      left_join(diccionario |>
                  select(llave,tema),
                by = c('aspecto' = 'llave' )) |>
      rename('respuesta' := .x,
             variable_principal= sexo)
  }) |>
  filter(respuesta == "Sí") |>
  rename(mean = media) |>
  filter(tema %in% principales_cand)

# Invertir variables
bd_conoce_per_sexo <-
  bd_conoce_per_sexo |>
      transmute(aux = variable_principal,
                variable_principal = tema,
                tema = aux,
                mean) |>
      select(!aux)

# Ver diferencias
bd_conoce_per_sexo <-
  bd_conoce_per_sexo |>
  group_by(variable_principal)|>
  mutate(mean_diff_pos = min(mean) + (max(mean)-min(mean))/2,
         mean_dif = (max(mean)-min(mean)))|>
  ungroup()


# conoce_per_sexo_graf <-
# bd_conoce_per_sexo |>
#   graficar_lolipop_diferencias(orden_variablePrincipal = rev(principales_cand),
#                                colores_variables_secundarias = c('Mujeres'= color_m,"Hombres"=color_h),
#                                nudge_x = 0.25,traslape = T,
#                                limite_dif_pct = 0.03,
#                                ajuste_pos = 0.007)+
#   labs(caption = p_conoce_per_tit)+
#   tema_morant() +
#   theme(legend.position = "bottom",
#         axis.text.x = element_text(size = 16),
#         axis.text.y = element_text(size = 16),
#         legend.text = element_text(size = 12),
#         plot.caption = element_text(size = 12))


conoce_per_sexo_graf <-
  bd_conoce_per_sexo |>
  mutate(cv= 0) |>
  graficar_crucePuntos(cruce = 'variable_principal',
                       vartype = 'cv',
                       variable = 'tema',
                       size_pct = 5,
                       orden_cruce = rev(principales_cand),traslape = T,limite_dif_pct = 0.3,ajuste_pos = 0.015)+
  scale_color_manual(values = c('Mujeres'= color_m,"Hombres"=color_h))+
  #scale_x_discrete(labels= c('F'='Mujer','M'='Hombre'))+
  scale_y_continuous(limits = c(0, .75),
                     labels = scales::percent)+
  labs(caption =p_conoce_per_tit)+
  theme(legend.position = 'bottom',
        legend.title = element_blank())


##### Conocimiento vs edad

bd_conoce_per_generacion <-
  conoce_per_vars |>
  purrr::map_df(.f = ~{


    bd_respuestas_efectivas |>
      select(all_of(.x),generacion,pesos) |>
      count(!!rlang::sym(.x),generacion,wt = pesos) |>
      mutate(media = n /sum(n)) |>
      mutate(aspecto = .x ) |>
      left_join(diccionario |>
                  select(llave,tema),
                by = c('aspecto' = 'llave' )) |>
      rename('respuesta' := .x,
             variable_principal= generacion)
  }) |>
  filter(respuesta == "Sí") |>
  group_by(tema) |>
  mutate(media = n/sum(n)) |>
  ungroup() |>
  rename(mean = media) |>
  filter(tema %in% principales_cand)

# Invertir variables
bd_conoce_per_generacion <-
  bd_conoce_per_generacion |>
  transmute(aux = variable_principal,
            variable_principal = tema,
            tema = aux,
            mean) |>
  select(!aux)

# Ver diferencias
bd_conoce_per_generacion <-
  bd_conoce_per_generacion |>
  group_by(variable_principal)|>
  mutate(mean_diff_pos = min(mean) + (max(mean)-min(mean))/2,
         mean_dif = (max(mean)-min(mean)))|>
  ungroup()


conoce_per_generacion_graf <-
  bd_conoce_per_generacion |>
  graficar_lolipop_diferencias(orden_variablePrincipal = rev(principales_cand),
                               colores_variables_secundarias = colores_generacion,
                               nudge_x = 0.25,
                               traslape = T,
                               limite_dif_pct = 0.03,
                               ajuste_pos = 0.007)+
  labs(caption = p_conoce_per_tit)+
  tema_morant() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        plot.caption = element_text(size = 12))+
  guides(color = guide_legend(ncol  = 2))







