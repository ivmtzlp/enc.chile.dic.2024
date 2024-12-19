## code to prepare `bd_entrevistas_efectivas` dataset goes here

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

Sys.setenv(tz = "America/Santiago")

source(file = "./R/utils_constantes.R")
source(file = "./R/fct_encuestar.R")
load(file = "./data/bd_region_comuna.rda")
load(file = "./data/bd_comunas_regionMetropolitanaSantiago.rda")

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
googledrive::drive_download(file = link_eliminadas,
                            path = path_eliminadas,
                            overwrite = T)

2

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
  left_join(bd_region_comuna, by = "comuna") |>
  mutate(comuna_mm = dplyr::if_else(condition = comuna %in% c("VALPARAISO", "VINA DEL MAR"),
                                    true = "VALPARAISO/VINA DEL MAR",
                                    false =  comuna),
         comuna_mm = dplyr::if_else(condition = comuna %in% c("COQUIMBO", "LA SERENA"),
                                    true = "COQUIMBO/LA SERENA",
                                    false =  comuna_mm),
         comuna_mm = dplyr::if_else(condition = comuna %in% c("CONCEPCION"),
                                    true = "GRAN CONCEPCION",
                                    false =  comuna_mm),
         comuna_mm = dplyr::if_else(condition = comuna %in% unique(bd_comunas_regionMetropolitanaSantiago$comuna),
                                    true = "SANTIAGO",
                                    false =  comuna_mm)) |>
  mutate(across(.cols = c(temas, medios_com, contains("_chile_O"),
                          interes_politica, interes_eleccion_mun_24, voto_pr, voto2_pr),
                .fns = ~ gsub(pattern = " \\(No leer\\)",
                              replacement = "",
                              x = .x)),
         across(.cols = c(medios_com, contains("_chile_O"),
                          interes_politica, interes_eleccion_mun_24, voto_pr, voto2_pr),
                .fns = ~ gsub(pattern = "Otro:",
                              replacement = "Otro",
                              x = .x)),
         across(.cols = c(starts_with("cali_")),
                .fns = ~ gsub(pattern = "99",
                              replacement = "Ns/Nc",
                              x = .x)),
         across(.cols = c(starts_with("cali_")),
                .fns = ~ as.factor(.x)))

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
  left_join(geolocalizacion_efectiva, by = "SbjNum")

shp_entrevistas_efectivas <-
  bd_respuestas_efectivas |>
  transmute(SbjNum,
            Srvyr,
            Date,
            manzana,
            comuna,
            comuna_mm,
            region,
            Latitude = GPS_INT_LA,
            Longitude = GPS_INT_LO) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4326)

# Exportar
usethis::use_data(shp_entrevistas_efectivas, overwrite = TRUE)
