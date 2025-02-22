


add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Participación política',
          location = ph_location_label(ph_label = "titulo"))

# if(modo != "solo nubes") {
#
#   add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
#     ph_with(value = g_medios_com,
#             location = ph_location_label(ph_label = "imagen_principal")) |>
#     ph_with(value = "La televisión y facebook son los medios de comuicación más utilizados",
#             location = ph_location_label(ph_label = "titulo"))
#
#   add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
#     ph_with(value = g_utiliza,
#             location = ph_location_label(ph_label = "imagen_principal")) |>
#     ph_with(value = "WhatsApp y Facebook son las redes sociales que más utilizan los chihuahuenses",
#             location = ph_location_label(ph_label = "titulo"))
#
# }

if(modo != "sin nubes") {

  # add_slide(pptx, layout = "gerencia_una_grafica_inteligencia", master = "gerencia") %>%
  #   ph_with(value = wc_prioridad_gob_maru,
  #           location = ph_location_label(ph_label = "imagen_principal")) |>
  #   ph_with(value = "El 25% de los entrevistados no perciben mejoras en el estado como resultado de la gestión del Gobierno de Maru Campos",
  #           location = ph_location_label(ph_label = "titulo"))

}

if(modo != "solo nubes") {



  # add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  #   ph_with(value = p_interes_eleccion_mun_24_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  #   ph_with(value = 'Interés en las elecciones municipales de 2024',
  #           location = ph_location_label(ph_label = "titulo"))
  #
  #
  # add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
  #   ph_with(value = p_participacion_pr_21_graf, location = ph_location_label(ph_label = "grafica_uno")) |>
  #   ph_with(value = p_participacion_mun_24_graf, location = ph_location_label(ph_label = "grafica_dos")) |>
  #   ph_with(value = 'Participación de las elecciones anteriores',
  #           location = ph_location_label(ph_label = "titulo"))

  # add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  #   ph_with(value = p_voto_proximas_elecciones_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  #   ph_with(value = 'Inclinación al voto para las próximas elecciones',
  #           location = ph_location_label(ph_label = "titulo"))
  #
  # add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  #   ph_with(value = p_participacion_primarias_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  #   ph_with(value = 'Intención a participación para las próximas elecciones primarias',
  #           location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
    ph_with(value = p_voto_proximas_elecciones_graf, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = p_participacion_primarias_graf, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = 'Inclinación al voto para las próximas elecciones',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
    ph_with(value = p_voto_pr_graf, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = p_voto2_pr_graf, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = 'Intención de voto para la Presidencia de Chile',
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = voto_pr_voto2_pr_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Segunda opción de voto de los 3 principales aspirantes de primera opción',
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = voto_pr_voto2_pr_v2_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Segunda opción de voto de los 2 principales aspirantes de de izquierda',
            location = ph_location_label(ph_label = "titulo"))

  # add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  #   ph_with(value = voto_pr_voto2_pr_ca_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  #   ph_with(value = 'Análisis de correspondencia entre primer y segunda opción de voto',
  #           location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_interes_politica_voto_pr_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Preferencia de voto y niveles de interés en la política',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = voto_pr_interes_politica_ac, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Análisis de correspondencia entre preferencia de voto y niveles de interés en la política',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_candidato_nunca_voto_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Candidato por el que nunca votaría',
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = voto_pr_candidato_nunca_voto_ca_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Análisis de correspondencia entre voto de primera opción contra aspirantes por los que nunca votaría',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_voto_proximas_elecciones_voto_pr_ominami_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Preferencia de voto e inclinación de perfil electoral',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_definicion_postura_ideologica_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Postura ideológica de los entrevistados',
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_identificacion_partido_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Identificación partidista',
            location = ph_location_label(ph_label = "titulo"))


}
