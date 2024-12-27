



add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Cualidades de un candidato ',
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

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_cualidades_valora_candidato_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Cualidades candidatos',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_necesita_chile_economia_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Preferencia de acción económica para el presidente',
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_necesita_chile_consenso_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Preferencia de acción social para el presidente',
            location = ph_location_label(ph_label = "titulo"))


}


add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Cualidades de un candidato ',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_aprueba_autoridades_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Evaluacion de autoridades',
          location = ph_location_label(ph_label = "titulo"))


