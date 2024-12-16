## code to prepare `cuotas_comunas` dataset goes here

# Preambulos ----------------------------------------------------------------------------------

library(dplyr)


# Data raw ------------------------------------------------------------------------------------

bd_tablas_diseno_muestral <-
  readxl::read_xlsx(path = "./data-raw/Tablas diseño muestral estudio elecciones.xlsx") |>
  janitor::clean_names() |>
  mutate(across(.cols = poblacion_estimada_de_la_capital,
                .fns = ~ gsub(pattern = "\\.|,",
                              replacement = "",
                              x = .x)),
         across(.cols = c(poblacion_estimada_de_la_capital),
                .fns = ~ as.integer(.x))) |>
  mutate(zona = dplyr::case_when(region %in% c("Arica y Parinacota",
                                               "Tarapacá",
                                               "Antofagasta",
                                               "Atacama",
                                               "Coquimbo") ~ "Zona 1",
                                 region %in% c("Metropolitana de Santiago") ~ "Zona 2",
                                 region %in% c("Valparaíso",
                                               "Libertador General Bernardo O'Higgins",
                                               "Maule") ~ "Zona 3",
                                 .default = "Zona 4"),
         region = stringr::str_to_upper(stringi::stri_trans_general(region, "Latin-ASCII")),
         comuna = stringr::str_to_upper(stringi::stri_trans_general(capital, "Latin-ASCII")))

bd_cuotas_comuna <-
  bd_tablas_diseno_muestral |>
  group_by(comuna) |>
  summarise(cuota = sum(muestra))

usethis::use_data(bd_cuotas_comuna, overwrite = TRUE)
