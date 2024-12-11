## code to prepare `shp_comuna` dataset goes here

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(sf)

devtools::load_all()

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
  # sf::st_simplify() |>
  mutate(COMUNA = stringr::str_to_upper(stringi::stri_trans_general(COMUNA, "Latin-ASCII"))) |>
  filter(COMUNA %in% comunas_visitadas)

# # Definir los límites para el recorte
# xlim <- c(-77, -65)
#
# # Crear un bbox con los límites
# bbox <- st_bbox(c(xmin = xlim[1],
#                   xmax = xlim[2],
#                   ymin = -56.53777,
#                   ymax = -17.4984),
#                 crs = st_crs(shp_comuna)
#                 )
#
# # Convertir el bbox a un polígono
# bbox_polygon <-
#   st_as_sfc(bbox)
#
# # Recortar el shape
# shp_comuna <-
#   st_intersection(shp_comuna, bbox_polygon)

usethis::use_data(shp_comuna, overwrite = TRUE)
