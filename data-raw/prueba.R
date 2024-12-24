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
    grupo_positivo = c("Negativa"),
    grupo_negativo = rev(c("Positiva")),
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
    ns_nc ="Ns/Nc",
    caption_nsnc = "Ns/Nc",
    size_caption_nsnc = 12
  )
