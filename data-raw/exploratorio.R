
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

devtools::load_all()

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

# Expportar -----------------------------------------------------------------------------------

bd_respuestas_efectivas |>
  openxlsx2::write_xlsx(file = "./data-raw/bd_efectivas_enc_chile_20241218.xlsx")
