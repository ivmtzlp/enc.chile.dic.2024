
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

devtools::load_all()

source(file = './data-raw/01_entrevistas_efectivas.R')

# Exploratorio --------------------------------------------------------------------------------

diccionario |>
  filter(llave == 'temas') |>
  tidyr::unnest(respuesta) |>
  distinct(respuesta)

bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(temas)

diccionario |>
  filter(bloque == "Contexto social") |>
  distinct(pregunta, llave)

diccionario |>
  filter(bloque == "Contexto social") |>
  distinct(llave) |>
  print(n = Inf)

bd_respuestas_efectivas |>
  glimpse()

# Expportar -----------------------------------------------------------------------------------

path_entrevistas_efectivas <-
  formato_archivo(nombre = "./data-raw/bd_efectivas_enc_chile",
                extension = "xlsx",
                tolerancia = 60)

bd_respuestas_efectivas |>
  openxlsx2::write_xlsx(file = path_entrevistas_efectivas)


bd_respuestas_efectivas |>
  distinct(participacion_primarias)
