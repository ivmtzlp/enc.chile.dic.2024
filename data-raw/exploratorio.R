
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

devtools::load_all()

# Exploratorio --------------------------------------------------------------------------------

bd_respuestas_efectivas |>
  as_tibble() |>
  count(participacion_primarias)
  select(participacion_primarias, voto_pr, voto2_pr)
