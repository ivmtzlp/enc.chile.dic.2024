## code to prepare `shp_comuna` dataset goes here

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(sf)

# devtools::load_all()

# Paths ---------------------------------------------------------------------------------------

wd_drive_chile <-
  "H:/Shared drives/Morant Consultores/Insumos/Chile/"

# Insumos -------------------------------------------------------------------------------------

comunas_visitadas <-
  bd_respuestas_efectivas |>
  distinct(comuna) |>
  pull()

shp_comuna <-
  sf::read_sf(paste0(wd_drive_chile, "Mapas/COMUNAS")) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = sf::st_crs(4326)) |>
  sf::st_make_valid() |>
  mutate(COMUNA = stringr::str_to_upper(stringi::stri_trans_general(COMUNA, "Latin-ASCII"))) |>
  filter(COMUNA %in% comunas_visitadas)

usethis::use_data(shp_comuna, overwrite = TRUE)
