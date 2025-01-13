
library(tidyverse)
library(leaflet)
#+-+-+-++-+-+-+-+-+-+-+Variables Insumos-+-+-+-+-+--+-+-+-+-+-+-+-

wd_drive <- "H:/Shared drives/Morant Consultores/Clientes/MarcoEnriquezOminami_Chile/Encuesta/Datos/"



#+-+-+-++-+-+-+-+-+-+-+Procesamiento mapa-+-+-+-+-+--+-+-+-+-+-+-+-

eliminadas_ubi <-
readxl::read_xlsx(path = path_eliminadas) |>
  filter(GPS == "Ubicación")

# readxl::read_xlsx(path = path_eliminadas) |>
#   filter(!is.na(GPS))



bd_elim_x_ubicacion <-
bd_respuestas_campo_raw |>
  filter(SbjNum %in%  (eliminadas_ubi |>
                         pull(ID)))




intentos_efectivos_elim_x_ubicacion <-
  bd_elim_x_ubicacion |>
  select(SbjNum, num_range("INT", 1:20)) |>
  mutate(across(.cols = !SbjNum, .fns = ~ as.character(.x))) |>
  tidyr::pivot_longer(cols = !SbjNum, names_to = "variable", values_to = "rechazo") |>
  filter(grepl(pattern = 'Iniciar entrevista', x = rechazo)) |>
  mutate(intento_efectivo = gsub(pattern = "INT", replacement = "", x = variable)) |>
  select(SbjNum, intento_efectivo)

geolocalizacion_efectiva_bd_elim_x_ubicacion <-
  purrr::pmap_df(.l = list(ids = intentos_efectivos_elim_x_ubicacion %>% pull(SbjNum),
                           intento_efectivo = intentos_efectivos_elim_x_ubicacion %>% pull(intento_efectivo)),
                 .f = ~ obtener_ubicacionEfectiva_surveyToGo(bd_respuestas = bd_elim_x_ubicacion,
                                                             id = ..1,
                                                             intento_efectivo = ..2))
bd_elim_x_ubicacion <-
  bd_elim_x_ubicacion |>
  left_join(geolocalizacion_efectiva_bd_elim_x_ubicacion, by = "SbjNum")




#################

map_entrevistas <-
  bd_elim_x_ubicacion|>
  select(SbjNum,Srvyr,comuna,Date,GPS_INT_LO,GPS_INT_LA)|>
  mutate(Longitude = as.numeric(GPS_INT_LO),
         Latitude = as.numeric(GPS_INT_LA))|>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)



leaflet::addMarkers()

leaflet()|>
  addTiles()|>
  addMarkers(
    data = map_entrevistas,
    label = paste(map_entrevistas$SbjNum),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )



st_write(map_entrevistas, paste0(wd_drive, "map_entrevistas_eliminadas.geojson"), driver = "GeoJSON")
st_write(map_entrevistas, paste0(wd_drive, "map_entrevistas_eliminadas.kml"), driver = "KML")







