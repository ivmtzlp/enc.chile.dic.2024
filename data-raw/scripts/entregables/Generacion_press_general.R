
# Preambulo -----------------------------------------------------------------------------------

library(officer)

# Constantes ----------------------------------------------------------------------------------
#source('./R/constantes_resultados.R')


# Resultados ----------------------------------------------------------------------------------

source(file = './data-raw/scripts/resultados/resultados_bloque_contexto_social.R')
source(file = './data-raw/scripts/resultados/resultados_bloque_conocimiento_personajes.R')
source(file = './data-raw/scripts/resultados/resultados_bloque_participacion_politica.R')
source(file = './data-raw/scripts/resultados/resultados_bloque_cualidades_candidato.R')
source(file = './data-raw/scripts/resultados/resultados_bloque_evaluacion_autoridades.R')
source(file = './data-raw/scripts/resultados/resultados_bloque_sociodemograficos.R')

#
# source(file = "./R/resultados_nubes_palabras.R")

# Parametros ----------------------------------------------------------------------------------

proyecto_fecha <- "Encuesta Nacional Chile - Diciembre 2024"

path_export <-
  encuestar:::formato_archivo(nombre = "./data-raw/press/enc_chile",
                              extension = "pptx",
                              tolerancia = 60)

dia <- lubridate::today() |> lubridate::day()
#dia <- "23"

c("completo", "sin nubes", "solo nubes")

modo <- "completo"

# Entregable ----------------------------------------------------------------------------------

# pptx <-
#   read_pptx(path = "./insumos/plantilla_general_09_12_24pptx")

pptx <-
  read_pptx(path = "./data-raw/plantilla_general_09_12_24.pptx")

## Introduccion -------------------------------------------------------------------------------

add_slide(pptx, layout = "gerencia_portada", master = "gerencia") %>%
  ph_with(value = 'Encuesta Nacional',
          location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = 'Chile',
          location = ph_location_label(ph_label = "subtitulo")) |>
  ph_with(value = paste0('Del 6 al ',dia,' de diciembre del 2024'),
          location = ph_location_label(ph_label = "periodo"))

#add_slide(pptx, layout = "gerencia_nota_metodologica", master = "gerencia")

## Problemáticas sociales  --------------------------------------------------------------------

source('./data-raw/scripts/entregables/entregable_bloque_contexto_social.R')


## Conocimiento de personajes ------------------------------------------------------------------

source('./data-raw/scripts/entregables/entregable_bloque_conocimiento_personajes.R')

## Participacion politica ------------------------------------------------------------------

source('./data-raw/scripts/entregables/entregable_bloque_participacion_politica.R')

## Cualidades de un candidato ------------------------------------------------------------------

source('./data-raw/scripts/entregables/entregable_bloque_cualidades_candidato.R')

## Evaluación de autoridades ------------------------------------------------------------------

source('./data-raw/scripts/entregables/entregable_bloque_evaluacion_autoridades.R')

## Sociodemograficos ------------------------------------------------------------------

source('./data-raw/scripts/entregables/entregable_bloque_sociodemograficos.R')


print(pptx, path_export)
