## code to prepare `diccionario` dataset goes here


# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

# Paths ---------------------------------------------------------------------------------------

link_diccionario <- "https://docs.google.com/spreadsheets/d/1vshJ0xAyTMwV0pEOhzF-458olUN5C8Kq-ubNmLlQFOI/edit?gid=0#gid=0"
path_diccionario <- "data-raw/diccionario_enc_chih_dic_2024.xlsx"

googledrive::drive_download(file = link_diccionario,
                            path = path_diccionario,
                            overwrite = T)

2

diccionario <-
  readxl::read_xlsx(path = path_diccionario) |>
  mutate(respuesta = strsplit(respuesta, "_"))

usethis::use_data(diccionario, overwrite = TRUE)
