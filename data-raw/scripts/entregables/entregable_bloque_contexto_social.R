



add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Contexto social',
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
    ph_with(value = g_temas, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Tema de mayor interés para la población',
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_medios_com, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Principales medios de información para los chilenos',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_utiliza, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Redes sociales más utilizadas por los chilenos',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_una_grafica_mas_100", master = "gerencia") %>%
    ph_with(value = g_problema_chile, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Principales problemas para la población chilena',
            location = ph_location_label(ph_label = "titulo"))


  # add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
  #   ph_with(value = g_cali_delincuencia, location = ph_location_label(ph_label = "grafica_uno")) |>
  #   ph_with(value = g_cali_educacion, location = ph_location_label(ph_label = "grafica_dos")) |>
  #   ph_with(value = 'Desempeño del gobierno de Gabriel Boric',
  #           location = ph_location_label(ph_label = "titulo"))
  #
  # add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
  #   ph_with(value = g_cali_salud, location = ph_location_label(ph_label = "grafica_uno")) |>
  #   ph_with(value = g_cali_empleo, location = ph_location_label(ph_label = "grafica_dos")) |>
  #   ph_with(value = 'Desempeño del gobierno de Gabriel Boric',
  #           location = ph_location_label(ph_label = "titulo"))
  #
  # add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
  #   ph_with(value = g_cali_pensiones, location = ph_location_label(ph_label = "grafica_uno")) |>
  #   ph_with(value = g_cali_ambiente, location = ph_location_label(ph_label = "grafica_dos")) |>
  #   ph_with(value = 'Desempeño del gobierno de Gabriel Boric',
  #           location = ph_location_label(ph_label = "titulo"))
  #
  # add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
  #   ph_with(value = g_cali_inmigracion, location = ph_location_label(ph_label = "grafica_uno")) |>
  #   ph_with(value = g_cali_derechosmujer, location = ph_location_label(ph_label = "grafica_dos")) |>
  #   ph_with(value = 'Desempeño del gobierno de Gabriel Boric',
  #           location = ph_location_label(ph_label = "titulo"))
  #
  # add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  #   ph_with(value = g_cali_economia, location = ph_location_label(ph_label = "imagen_principal")) |>
  #   ph_with(value = 'Desempeño del gobierno de Gabriel Boric',
  #           location = ph_location_label(ph_label = "titulo"))
  #
  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_cali_desem_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Desempeño del gobierno de Gabriel Boric',
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
    ph_with(value = g_chile_actual, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = g_chile_futuro, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = 'Perspectiva de mejora en la vida de los chilenos',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
    ph_with(value = g_frases_ricos, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = g_frases_gobierno, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = 'Opinión respecto a temas de coyuntura de los chilenos',
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_satisfaccion_democracia, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Percepción de la democarcia de los chilenos',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = escala_izq_der_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Posicionamiento político de los chilenos frente a temas sociales',
            location = ph_location_label(ph_label = "titulo"))


##############3

}
