
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(openxlsx)
Sys.setenv(tz = "America/Santiago")

# Insumos -------------------------------------------------------------------------------------

# source(file = "./data-raw/01_entrevistas_efectivas.R")

bd_respuestas_auditoria <-
  bd_respuestas_efectivas |>
  transmute(encuestador = Srvyr,
            region,
            comuna,
            fecha)

# Base de entrevistas por encuestador ---------------------------------------------------------

bd_encuestadores_dia <-
  bd_respuestas_auditoria |>
  as_tibble() |>
  group_by(encuestador) |>
  count(fecha) |>
  arrange(fecha) |>
  ungroup() |>
  tidyr::pivot_wider(id_cols = encuestador,
                     names_from = fecha,
                     values_from = n) |>
  mutate(across(.cols = !c(encuestador),
                .fns = ~ tidyr::replace_na(data = .x, replace = 0))) |>
  mutate(acum = rowSums(across(.cols = !encuestador)))

# Simulacion de rechazo

bd_simuacion <-
  bd_encuestadores_dia |>
  tidyr::pivot_longer(cols = !encuestador,
                      names_to = "fecha",
                      values_to = "entrevistas") |>
  filter(fecha == lubridate::as_date(lubridate::today() - lubridate::days(1))) |>
  left_join(bd_respuestas_efectivas |>
              filter(fecha == lubridate::as_date(lubridate::today() - lubridate::days(1))) |>
              select(SbjNum, Srvyr, intento_efectivo) |>
              group_by(Srvyr) |>
              count(intento_efectivo, name = "intentos_inmediatos") |>
              filter(intento_efectivo == 1) |>
              ungroup(),
            by = c("encuestador" = "Srvyr")) |>
  mutate(mitad_efectivas = round(entrevistas/2)) |>
  filter(mitad_efectivas <= intentos_inmediatos) |>
  select(!intento_efectivo) |>
  transmute(encuestador,
            posible_simulación = 'Sí')

bd_encuestadores_dia <-
  bd_encuestadores_dia |>
  left_join(bd_simuacion) |>
  rename(Acumulado = acum,
         'Sin registro de rechazos' = posible_simulación)

bd_region_dia <-
  bd_respuestas_auditoria |>
  as_tibble() |>
  group_by(region, encuestador) |>
  count(fecha) |>
  arrange(fecha) |>
  ungroup() |>
  tidyr::pivot_wider(id_cols = c(region, encuestador),
                     names_from = fecha,
                     values_from = n) |>
  mutate(across(.cols = !c(region, encuestador),
                .fns = ~ tidyr::replace_na(data = .x, replace = 0))) |>
  mutate(acum = rowSums(across(.cols = !c(region, encuestador)))) |>
  rename(Acumulado = acum)

bd_comuna_dia <-
  bd_respuestas_auditoria |>
  as_tibble() |>
  group_by(region, comuna, encuestador) |>
  count(fecha) |>
  arrange(fecha) |>
  ungroup() |>
  tidyr::pivot_wider(id_cols = c(region, comuna, encuestador),
                     names_from = fecha,
                     values_from = n) |>
  mutate(across(.cols = !c(region, comuna, encuestador),
                .fns = ~ tidyr::replace_na(data = .x, replace = 0))) |>
  mutate(acum = rowSums(across(.cols = !c(region, comuna, encuestador)))) |>
  rename(Acumulado = acum)

## Exportar -----------------------------------------------------------------------------------

output_file_path = formato_archivo(nombre = "./data-raw/bd_reportes/reporte_avances",
                                               extension = "xlsx",
                                               tolerancia = 60)

# Create a workbook
wb <- createWorkbook()

# Add a worksheet to the workbook
addWorksheet(wb, "Comunas")
addWorksheet(wb, "Regiones")
addWorksheet(wb, "Encuestadores")

# Write the dataframe to the worksheet
writeData(wb, "Encuestadores", bd_encuestadores_dia)
writeData(wb, "Regiones", bd_region_dia)
writeData(wb, "Comunas", bd_comuna_dia)

saveWorkbook(wb, output_file_path, overwrite = TRUE)
print("Se ha guardado el archivo xlsx de forma exitosa")

# Base de efectivas con geolocalizacon --------------------------------------------------------

bd_respuestas_efectivas_export <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(id = SbjNum,
         Encuestador = Srvyr,
         Fecha = Date,
         Manzana = manzana,
         Comuna = comuna,
         nombre = Q_131,
         celular = Q_129,
         GPS_INT1_LA:GPS_INT16_DATE,
         intento_final = INT,
         GPS_INT_LA,
         GPS_INT_LO) |>
  mutate(link_map =  paste0("https://www.google.com/maps?q=", GPS_INT_LA, ",", GPS_INT_LO,"&t=m"))

output_bd_efectivas_path = formato_archivo(nombre = "./data-raw/bd_reportes/bd_efectivas",
                                                       extension = "xlsx",
                                                       tolerancia = 60)

openxlsx::write.xlsx(x = bd_respuestas_efectivas_export,
                     file = output_bd_efectivas_path)
