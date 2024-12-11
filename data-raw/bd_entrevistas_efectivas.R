## code to prepare `bd_entrevistas_efectivas` dataset goes here

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
devtools::load_all()

# Paths ---------------------------------------------------------------------------------------

link_eliminadas <- "https://docs.google.com/spreadsheets/d/1kzB6JoLrJ-79WrxJkRbRtCfiPgAIKaTI/edit?gid=812542906#gid=812542906"
path_eliminadas <- "data-raw/bd_eliminadas_enc_chih_dic_2024.xlsx"

# Data raw ------------------------------------------------------------------------------------

## Entrevistas de campo -----------------------------------------------------------------------

bd_respuestas_campo_raw <-
  openxlsx2::read_xlsx(file = "./data-raw/bd_respuestas_enc_chile_dic_2024.xlsx", na.strings = "-1") |>
  as_tibble() |>
  mutate(fecha = lubridate::as_date(Date))

## Eliminadas ---------------------------------------------------------------------------------

# Eliminadas
googledrive::drive_auth(path = "./data-raw/api-key.json")
googledrive::drive_download(file = link_eliminadas,
                            path = path_eliminadas,
                            overwrite = T)

bd_eliminadas <-
  readxl::read_xlsx(path = path_eliminadas) |>
  transmute(SbjNum = ID,
            razon = Observaciones)

# Base de entrevistas efectivas ---------------------------------------------------------------

bd_respuestas_efectivas <-
  bd_respuestas_campo_raw |>
  filter(!SbjNum %in% bd_eliminadas$SbjNum) |>
  # Kath simula entrevistas para inflar sus resultados jaja
  filter(!Srvyr %in% c("KATHERYN HERNANDEZ",
                       "KATHERYN HERNANDEZ ")) |>
  # La base de region comuna se obtiene a partir de otro script. Es un insumo de sólo lectura
  left_join(bd_region_comuna, by = "comuna")

# Calculo de registros de rechazo -------------------------------------------------------------

intentos_efectivos <-
  bd_respuestas_efectivas |>
  select(SbjNum, num_range("INT", 1:20)) |>
  mutate(across(.cols = !SbjNum, .fns = ~ as.character(.x))) |>
  tidyr::pivot_longer(cols = !SbjNum, names_to = "variable", values_to = "rechazo") |>
  filter(grepl(pattern = 'Iniciar entrevista', x = rechazo)) |>
  mutate(intento_efectivo = gsub(pattern = "INT", replacement = "", x = variable)) |>
  select(SbjNum, intento_efectivo)

geolocalizacion_efectiva <-
  purrr::pmap_df(.l = list(ids = intentos_efectivos %>% pull(SbjNum),
                           intento_efectivo = intentos_efectivos %>% pull(intento_efectivo)),
                 .f = ~ obtener_ubicacionEfectiva_surveyToGo(bd_respuestas = bd_respuestas_efectivas,
                                                             id = ..1,
                                                             intento_efectivo = ..2))

bd_respuestas_efectivas <-
  bd_respuestas_efectivas |>
  left_join(geolocalizacion_efectiva, by = "SbjNum") |>
  mutate(Latitude = GPS_INT_LA,
         Longitude = GPS_INT_LO) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4326)

# Exportar
usethis::use_data(bd_respuestas_efectivas, overwrite = TRUE)
