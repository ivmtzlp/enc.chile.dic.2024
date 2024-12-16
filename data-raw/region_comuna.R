## code to prepare `region_comuna` dataset goes here

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

# Paths ---------------------------------------------------------------------------------------

wd_drive_chile <- "H:/Shared drives/Morant Consultores/Insumos/Chile"

# Insumos -------------------------------------------------------------------------------------

bd_gb_21 <-
  readxl::read_xlsx(path = paste0(wd_drive_chile, "/Electorales/gb_21.xlsx"), skip = 4) |>
  janitor::clean_names()

# Obtener relacion region comuna --------------------------------------------------------------

bd_region_comuna <-
  bd_gb_21 |>
  distinct(region, comuna)

usethis::use_data(bd_region_comuna, overwrite = TRUE)

bd_comunas_regionMetropolitanaSantiago <-
  bd_region_comuna |>
  filter(region == "METROPOLITANA DE SANTIAGO") |>
  mutate(comuna = stringr::str_to_upper(stringi::stri_trans_general(comuna, "Latin-ASCII")))

usethis::use_data(bd_comunas_regionMetropolitanaSantiago, overwrite = TRUE)
