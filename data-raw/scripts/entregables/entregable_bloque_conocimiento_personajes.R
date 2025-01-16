


# source(file = "R/resultados_bloque_problematicas_sociales.R")

add_slide(pptx, layout = "gerencia_subportada", master = "gerencia") %>%
  ph_with(value = 'Conocimiento de personajes ',
          location = ph_location_label(ph_label = "titulo"))




if(modo != "solo nubes") {

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_conoce_per_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Conocimiento de personajes',
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_opinion_per_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Opinión de personajes',
            location = ph_location_label(ph_label = "titulo"))
}


if(modo != "sin nubes") {



  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
    ph_with(value = wc_razon_opinion_bachelet_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = wc_razon_opinion_bachelet_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = "¿Porqué tiene una opinión (...) de Michelle Bachelet?",
            location = ph_location_label(ph_label = "titulo"))



  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
    ph_with(value = wc_razon_opinion_mathei_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = wc_razon_opinion_mathei_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = "¿Porqué tiene una opinión (...) de Evelyn Mathei?",
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
    ph_with(value = wc_razon_opinion_ominami_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = wc_razon_opinion_ominami_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = "¿Porqué tiene una opinión (...) de Marco Enríquez-Ominami?",
            location = ph_location_label(ph_label = "titulo"))


  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
    ph_with(value = wc_razon_opinion_toha_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = wc_razon_opinion_toha_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = "¿Porqué tiene una opinión (...) de Carolina Tohá?",
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
    ph_with(value = wc_razon_opinion_vodanovic_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = wc_razon_opinion_vodanovic_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = "¿Porqué tiene una opinión (...) de Tomás Vodanovic?",
            location = ph_location_label(ph_label = "titulo"))

  add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
    ph_with(value = wc_razon_opinion_winter_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
    ph_with(value = wc_razon_opinion_winter_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
    ph_with(value = "¿Porqué tiene una opinión (...) de Gonzalo Winter?",
            location = ph_location_label(ph_label = "titulo"))

  ##################

  # add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
  #   ph_with(value = wc_razon_opinion_parisi_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
  #   ph_with(value = wc_razon_opinion_parisi_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
  #   ph_with(value = "¿Porqué tiene una opinión (...) de Franco Parisi?",
  #           location = ph_location_label(ph_label = "titulo"))
  #
  #
  # add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
  #   ph_with(value = wc_razon_opinion_kast_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
  #   ph_with(value = wc_razon_opinion_kast_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
  #   ph_with(value = "¿Porqué tiene una opinión (...) de José Antonio Kast?",
  #           location = ph_location_label(ph_label = "titulo"))
  #
  # add_slide(pptx, layout = "gerencia_dos_graficas_equitativas_inteligencia", master = "gerencia") %>%
  #   ph_with(value = wc_razon_opinion_kaiser_positiva, location = ph_location_label(ph_label = "grafica_uno")) |>
  #   ph_with(value = wc_razon_opinion_kaiser_negativa, location = ph_location_label(ph_label = "grafica_dos")) |>
  #   ph_with(value = "¿Porqué tiene una opinión (...) de Johannes Kaiser?",
  #           location = ph_location_label(ph_label = "titulo"))
  #

}


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = p_opinion_ominami_interes_politica_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = 'Opinión de Marco Enríquez Ominami por nivel de interés en la política',
          location = ph_location_label(ph_label = "titulo"))


if(modo != "solo nubes") {
  add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
    ph_with(value = p_calif_per_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
    ph_with(value = 'Evaluación del trabajo de los personajes',
            location = ph_location_label(ph_label = "titulo"))


}
