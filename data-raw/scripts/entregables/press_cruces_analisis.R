




# Preambulo -----------------------------------------------------------------------------------

library(officer)




# Parametros ----------------------------------------------------------------------------------

proyecto_fecha <- "Encuesta Nacional Chile - Diciembre 2024"

path_export <-
  encuestar:::formato_archivo(nombre = "./data-raw/press/analisis_general_ominami",
                              extension = "pptx",
                              tolerancia = 60)

dia <- lubridate::today() |> lubridate::day()




pptx <-
  read_pptx(path = "./data-raw/plantilla_general_09_12_24.pptx")


add_slide(pptx, layout = "gerencia_portada", master = "gerencia") %>%
  ph_with(value = 'Encuesta Nacional Chile',
          location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = 'Análisis generales de coyontura',
          location = ph_location_label(ph_label = "subtitulo")) |>
  ph_with(value = paste0('Del 6 de diciembre al ',dia,' de enero del 2024'),
          location = ph_location_label(ph_label = "periodo"))

source(file = './data-raw/scripts/resultados/resultados_bloque_voto_ominami.R')
source(file = './data-raw/scripts/resultados/resultados_bloque_opi_bue_mal_no_vot_ominami.R')
source(file = './data-raw/scripts/resultados/analisis_coyuntaral_meo.R')



print(pptx, path_export)







