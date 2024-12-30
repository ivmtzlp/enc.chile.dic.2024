


add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Datos Sociodemográficos',
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
    ph_with(value = g_educacion_jefe_hogar, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Educación del jefe/a del hogar',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_ocupacion_jefe_hogar, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Ocupación del jefe/a del hogar',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_personas_viven_hogar, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Personas que viven en el hogar',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_ingreso_mensual_hogar, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Ingreso mensual del hogar',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_vivienda_comuna, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Pertenencia en el hogar',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_asiste_educacion, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Educación formal actual',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = g_asiste_educacion, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Educación formal actual',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
    ph_with(value = g_grado_curso_aprobado, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = g_curso_aprobado, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = 'Relación del último grado aprobado',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_perteneciente_pueblo_indigena_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Pertenencia a un pueblo originario',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
    ph_with(value = p_semana_pasada_trabajo_graf, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = g_razon_no_trabajo, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = 'Estatus laboral de la persona entrevistada',
            location = ph_location_label(ph_label = "titulo"))

  ##############3

}
