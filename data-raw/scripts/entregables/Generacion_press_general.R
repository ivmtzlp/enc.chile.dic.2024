
# Preambulo -----------------------------------------------------------------------------------

library(officer)

# Constantes ----------------------------------------------------------------------------------
#source('./R/constantes_resultados.R')


# Resultados ----------------------------------------------------------------------------------

# source(file = "./R/resultados_bloque_problematicas_sociales.R")
# source(file = "./R/resultados_bloque_evaluacion_autoridades.R")
# source(file = './R/resultados_bloque04_1_Candidato_ideal.R')
# source(file = './R/resultados_bloque02_MetodologiaMorena.R')
# source(file = './R/resultados_bloque02_5_Analisis_perfiles.R')
# source(file = './R/resultados_bloque04_Intencion_gobernatura.R')
# source(file = "./R/resultados_bloque_conocimiento_personajes_secundarios.R")
# source(file = "./R/resultados_bloque_programas_sociales.R")
# source(file = './R/resultados_bloque07_datosSociodemograficos.R')
# source(file = "./R/resultados_bloque_amai.R")
# source(file = "./R/Grafica_redes_personajes.R")
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

## Participacion politica ------------------------------------------------------------------

source('./data-raw/scripts/entregables/entregable_bloque_participacion_politica.R')

## Cualidades de un candidato ------------------------------------------------------------------

source('./data-raw/scripts/entregables/entregable_bloque_cualidades_candidato.R')

## Evaluación de autoridades ------------------------------------------------------------------

#source('./R/entregable_bloque_evaluacion_autoridades.R')



print(pptx, path_export)
