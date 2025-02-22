## code to prepare `bd_entrevistas_efectivas` dataset goes here

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

Sys.setenv(tz = "America/Santiago")

source(file = "./R/utils_constantes.R")
source(file = "./R/fct_encuestar.R")
source(file = "./R/fct_resultados.R")
load(file = "./data/bd_region_comuna.rda")
load(file = "./data/bd_comunas_regionMetropolitanaSantiago.rda")

# Paths ---------------------------------------------------------------------------------------

link_eliminadas <- "https://docs.google.com/spreadsheets/d/1kzB6JoLrJ-79WrxJkRbRtCfiPgAIKaTI/edit?gid=812542906#gid=812542906"
path_eliminadas <- "data-raw/bd_eliminadas_enc_chih_dic_2024.xlsx"

link_sexos <- "https://docs.google.com/spreadsheets/d/13xdlT5w-EUmfKG80iV_Cy465Dt7hsavE/edit?usp=sharing&ouid=107325048037311002262&rtpof=true&sd=true"
path_sexos <- "data-raw/bd_sexos_enc_chih_dic_2024.xlsx"


link_sexos_v2 <- "https://docs.google.com/spreadsheets/d/14QrBN6azj2r6XXmkWpKFt2IhRXYsTvOK/edit?usp=drive_link&ouid=107325048037311002262&rtpof=true&sd=true"
path_sexos_v2 <- "data-raw/bd_sexos_enc_chih_dic_2024_v2.xlsx"


link_sustitucioenes <- "https://docs.google.com/spreadsheets/d/1qio4TWIypZaiAfe8a5KvX1QesCeRaHuOXO2fjvkZhwM/edit?usp=sharing"
path_sustitucioenes <- "data-raw/sustituciones.xlsx"


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

## Sexos faltantes  ---------------------------------------------------------------------------------

googledrive::drive_download(file = link_sexos,
                            path = path_sexos,
                            overwrite = T)

2


googledrive::drive_download(file = link_sexos_v2,
                            path = path_sexos_v2,
                            overwrite = T)

2


bd_sexos_faltantes <-
  readxl::read_xlsx(path = path_sexos) |>
  rename(edad_falt = edad,
         sexo_falt = sexo) |>
  bind_rows(readxl::read_xlsx(path = path_sexos_v2) |>
              mutate(edad = as.numeric(edad)) |>
              # select(edad,sexo)
              rename(edad_falt = edad,
                     sexo_falt = sexo)
              )

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
  mutate(voto_pr =  ifelse(is.na(voto_pr),"Ns/Nc",voto_pr)) |>
  mutate(voto2_pr =  ifelse(is.na(voto_pr),"Ns/Nc",voto2_pr)) |>
  #Bloque AMAI
  mutate(generacion = case_when(edad >= 18 & edad <= 25 ~ "Generación Z (18 a 25 años)",
                                edad >= 26 & edad <= 40 ~ "Millenials (26 a 40 años)",
                                edad >= 41 & edad <= 55 ~ "Generación X (41 a 55 años)",
                                edad >= 56  ~ "Baby Boomers  (56 años o más)"),
         generacion = factor(generacion, levels = c("Generación Z (18 a 25 años)",
                                                    "Millenials (26 a 40 años)",
                                                    "Generación X (41 a 55 años)",
                                                    "Baby Boomers  (56 años o más)"))) |>
  mutate(across(.cols = c(temas, medios_com, contains("_chile_O"),
                          chile_actual, chile_futuro,
                          frases_ricos, frases_gobierno,
                          satisfaccion_democracia,
                          interes_politica, interes_eleccion_mun_24, voto_pr, voto2_pr,
                          voto_proximas_elecciones,opinion_primarias,candidato_nunca_voto,
                          definicion_postura_ideologica,aprueba_gobierno_boric,aprueba_ministros,
                          ingreso_mensual_hogar),
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
         across(.cols = c(starts_with("escala_")),
                .fns = ~ gsub(pattern = "ns/nc",
                              replacement = "Ns/Nc",
                              x = .x)),
         across(.cols = c(starts_with("cali_")),
                .fns = ~ as.factor(.x))) |>
  mutate(rango_edad = case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                edad >= 30 & edad <= 39 ~ "30-39",
                                edad >= 40 & edad <= 49 ~ "40-49",
                                edad >= 50 & edad <= 59 ~ "50-59",
                                edad >= 60 & edad <= 64 ~ "60-64",
                                edad >= 65 ~ "65+",
                                T ~ NA)) |>
  mutate(pesos = 1)|>
  mutate(manzana =  gsub("\\.0","",manzana)) |>
  mutate(grupo_edad = cut(as.numeric(edad),
                          breaks = c(18, 29, 44, 59, Inf),
                          labels = c("18-29", "30-44", "45-59", "60 y más"),
                          right = FALSE))

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

# Agregar categorias -------------------------------------------------------------

source(file = "./data-raw/scripts/nubes/03_bds_categorias_procesadas.R")

bd_respuestas_efectivas <-
  bd_respuestas_efectivas |>
  left_join(bd_categorias_procesada, by = c("SbjNum"="id"))

# Agregar sexos faltantes -----------------------------------------------------

bd_respuestas_efectivas <-
bd_respuestas_efectivas |>
  left_join(bd_sexos_faltantes |>
              select(SbjNum ,sexo_falt),
            by = "SbjNum") |>
  mutate(sexo = ifelse(is.na(sexo),sexo_falt,sexo))

bd_respuestas_efectivas <-
  bd_respuestas_efectivas |>
  filter(!is.na(sexo)) |>
  mutate(sexo = ifelse(sexo == "-","Mujer",sexo))

# Sustituciones -----------------------------------------------------
googledrive::drive_download(file = link_sustitucioenes,
                            path = path_sustitucioenes,
                            overwrite = T)

2

bd_sustituciones <-
  readxl::read_xlsx(path = path_sustitucioenes)

lista_codigo_survey<- bd_sustituciones |>
  janitor::clean_names() |>
  distinct(codigo_survey) |> pull()

bd_sustituciones_pivot<-
  bd_sustituciones |>
  janitor::clean_names() |>
  rename(SbjNum=id) |>
  select(SbjNum,codigo_survey ,correcion ) |>
  tidyr::pivot_wider(names_from = codigo_survey,values_from = correcion)


for (variable_survey in lista_codigo_survey) {
  bd_respuestas_efectivas <-
    bd_respuestas_efectivas |>
    left_join(bd_sustituciones_pivot |>
                select(all_of(c("SbjNum",variable_survey))) |>
                rename("aux"=variable_survey),
              by = "SbjNum") |>
    mutate(!!rlang::sym(variable_survey) := ifelse(!is.na(aux), aux, !!rlang::sym(variable_survey))) %>% # Sustituir si hay un valor en df2
    select(-aux)
}



# Agregar nivel socieconomico ---------------------
# matriz_nive_eco <- readxl::read_excel("./data-raw/bd_genericas/educ_vs_trab.xlsx", sheet = "Sheet1")
#
#
# matriz_nive_eco <-
#   matriz_nive_eco %>%
#   tidyr::pivot_longer(cols = -1, names_to = "ocupacion_jefe_hogar_redux", values_to = "clasif_eco") %>%
#   rename(educacion_jefe_hogar_redux = 1)
#
# bd_respuestas_efectivas <-
#   bd_respuestas_efectivas |>
#   mutate(ocupacion_jefe_hogar_redux = case_when(
#     grepl("Trabajos menores",ocupacion_jefe_hogar) ~ "Trabajos menores",
#     grepl("Oficio menor, obrero no calificado",ocupacion_jefe_hogar) ~ "Oficio menor, obrero no calificado",
#     grepl("Obrero calificado",ocupacion_jefe_hogar) ~ "Obrero calificado",
#     grepl("Empleado administrativo medio y bajo",ocupacion_jefe_hogar) ~ "Empleado administrativo medio y bajo",
#     grepl("Ejecutivo medio, ejecutivo medio-alto",ocupacion_jefe_hogar) ~ "Ejecutivo medio, ejecutivo medio-alto",
#     grepl("Alto ejecutivo",ocupacion_jefe_hogar) ~ "Alto ejecutivo"
#   )  ) |>
#   mutate(ocupacion_jefe_hogar_redux = factor(ocupacion_jefe_hogar_redux,levels = c("Trabajos menores","Oficio menor, obrero no calificado","Obrero calificado",
#                                                                                    "Empleado administrativo medio y bajo","Ejecutivo medio, ejecutivo medio-alto",
#                                                                                    "Alto ejecutivo") )) |>
#
#   mutate(educacion_jefe_hogar_redux = case_when(
#     grepl("Educación básica incompleta",educacion_jefe_hogar) ~ "Educación básica incompleta",
#     grepl("Básica completa",educacion_jefe_hogar) ~ "Básica completa",
#     grepl("Media incompleta",educacion_jefe_hogar) ~ "Media incompleta",
#     grepl("Media completa ",educacion_jefe_hogar) ~ "Media completa",
#     grepl("Técnica completa",educacion_jefe_hogar) ~ "Técnica completa",
#     grepl("Universitaria completa",educacion_jefe_hogar) ~ "Universitaria completa",
#     grepl("Post Grado",educacion_jefe_hogar) ~ "Post Grado"
#   )  ) |>
#   mutate(educacion_jefe_hogar_redux = factor(educacion_jefe_hogar_redux,levels = c("Educación básica incompleta","Básica completa","Media incompleta",
#                                                                                    "Media completa","Técnica completa","Universitaria completa","Post Grado")
#   ))
#
#
# bd_respuestas_efectivas <-
#   bd_respuestas_efectivas %>%
#   left_join(matriz_nive_eco,
#             by = c("ocupacion_jefe_hogar_redux", "educacion_jefe_hogar_redux")) |>
#   mutate(niv_soc = case_match(clasif_eco,
#                               "CB"~"C3",
#                               "CA"~"C2",
#                               c("A","B")~"ABC1",
#                               .default = clasif_eco
#                               )
#            )


# Agregar pesos  -----------------------------------------------------
#

diseno_sn_pesp <- survey::svydesign(ids = ~1,
                                    data = bd_respuestas_efectivas |>
                                      filter(!is.na(sexo)),
                                    strata = ~ sexo + rango_edad + comuna_mm,
                                    weights = ~pesos

)


population_totals <- readRDS("./data-raw/bd_genericas/vector_de_pesos.rds")

calibrated_design <- survey::calibrate(
  diseno_sn_pesp,
  formula = ~sexo + rango_edad + comuna_mm,
  population = population_totals
)

bd_respuestas_efectivas <-
  bd_respuestas_efectivas |>
  filter(!is.na(sexo)) |>
  mutate(pesos = weights(calibrated_design))

#correcionmanzanas

bd_respuestas_efectivas <-
  bd_respuestas_efectivas |>
  mutate(manzana =  gsub("\\.0","",manzana))




# shps efectivas -------------------------------------------------------------
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
