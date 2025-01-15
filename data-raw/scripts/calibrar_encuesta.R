

library(tidyverse)
library(survey)
library(srvyr)


vector <- function(tb, variable){
  tb[[2]] |> purrr::set_names(paste0(variable,tb[[1]]))
}

comunas_encuesta <- bd_respuestas_efectivas |>
  distinct(comuna) |>
  pull(comuna)

diccionario |> view()

#####################################################

bd_comunas_exist<-
bd_respuestas_efectivas |>
  distinct(comuna_mm,comuna)
#######################################################

# SIN PESOS


diseno_sn_pesp <- survey::svydesign(ids = ~1,
                                    data = bd_respuestas_efectivas |>
                                      mutate(rango_edad = case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                                                    edad >= 30 & edad <= 39 ~ "30-39",
                                                                    edad >= 40 & edad <= 49 ~ "40-49",
                                                                    edad >= 50 & edad <= 59 ~ "50-59",
                                                                    edad >= 60 & edad <= 64 ~ "60-64",
                                                                    edad >= 65 ~ "65+",
                                                                    T ~ NA)) |>
                                               filter(!is.na(sexo)),
                                    weights = ~pesos

                                      )




diseno_sn_pesp <- survey::svydesign(ids = ~1,
                                    data = bd_respuestas_efectivas |>
                                      mutate(rango_edad = case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                                                    edad >= 30 & edad <= 39 ~ "30-39",
                                                                    edad >= 40 & edad <= 49 ~ "40-49",
                                                                    edad >= 50 & edad <= 59 ~ "50-59",
                                                                    edad >= 60 & edad <= 64 ~ "60-64",
                                                                    edad >= 65 ~ "65+",
                                                                    T ~ NA)) |>
                                      filter(!is.na(sexo)),
                                    strata = ~ sexo + rango_edad + comuna_mm,
                                    weights = ~pesos

)


svymean(x = ~voto_pr,
        design = diseno_sn_pesp,
        na.rm = T)

# diseno_sn_pesp_svyr<- as_survey_design(diseno_sn_pesp)
#
# diseno_sn_pesp_svyr |>
#   group_by(voto_pr) |>
#   summarise(pct = survey_mean(na.rm = T) )


encuestar:::analizar_frecuencias(diseno = diseno_sn_pesp,pregunta = "voto_pr")


####################################################################



censo_tot_comu_sexo_edad<-
  read.csv('./data-raw/poblacion_censo_sexo_edad__comuna_chile.csv') |>
  as_tibble()


bd_sexo_enc_censo_peso <-
  censo_tot_comu_sexo_edad |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |> # distinct(NOM_COMUNA)
  filter(edad >=18) |>
  mutate(rango_edad =  case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                 edad >= 30 & edad <= 39 ~ "30-39",
                                 edad >= 40 & edad <= 49 ~ "40-49",
                                 edad >= 50 & edad <= 59 ~ "50-59",
                                 edad >= 60 & edad <= 64 ~ "60-64",
                                 edad >= 65 ~ "65+",
                                 T ~ NA)) |>
  left_join(
    bd_respuestas_efectivas |>
      distinct(comuna_mm,comuna),
    by = c("NOM_COMUNA" = "comuna")
  ) |>
  group_by(comuna_mm,sexo_nom,rango_edad ) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  rename(sexo = sexo_nom)


comuna_mm_censo_peso_vec <-
bd_sexo_enc_censo_peso |>
  group_by(comuna_mm) |>
  summarise(totales = sum(poblacion)) |>
  distinct() |>
  vector("comuna_mm")

rango_edad_censo_peso_vec <-
bd_sexo_enc_censo_peso |>
  group_by(rango_edad) |>
  summarise(totales = sum(poblacion)) |>
  distinct() |>
  vector("rango_edad")

sexo_enc_censo_peso_vec <-
bd_sexo_enc_censo_peso |>
  group_by(sexo) |>
  summarise(totales = sum(poblacion)) |>
  distinct() |>
  vector("sexo")


# Rectificacion de totales
sum(comuna_mm_censo_peso_vec)
sum(rango_edad_censo_peso_vec)
sum(sexo_enc_censo_peso_vec)

# Vector de calibracion


population_totals <- sum(comuna_mm_censo_peso_vec)

# names(population_totals)[1] <- "(Intercept)"


#saveRDS(population_totals, file = "./data-raw/bd_genericas/vector_de_pesos.rds")

# Cargarlo de nuevo
population_totals_2 <- readRDS("./data-raw/bd_genericas/vector_de_pesos.rds")







population_totals <- c(population_totals,comuna_mm_censo_peso_vec,
  rango_edad_censo_peso_vec,
  sexo_enc_censo_peso_vec)



calibrated_design <- calibrate(
  diseno_sn_pesp,
  formula = ~sexo + rango_edad + comuna_mm,
  population = population_totals
)



encuestar:::analizar_frecuencias(diseno = diseno_sn_pesp,pregunta = "voto_pr") |>
  mutate(tipo_diseno = "Sin pesos") |>
  bind_rows(encuestar:::analizar_frecuencias(diseno = calibrated_design,pregunta = "voto_pr") |>
              mutate(tipo_diseno = "Calibr. sexo + rango_edad + comuna")) |>
  mutate(tipo_diseno = factor(tipo_diseno, levels= c("Sin pesos",
                                                     "Calibr. sexo + rango_edad + comuna"))) |>
  encuestar:::graficar_barras(orden_respuestas = 1)+
  facet_wrap(~tipo_diseno)+
  tema_morant()




encuestar:::analizar_frecuencias(diseno = diseno_sn_pesp,pregunta = "voto2_pr") |>
  mutate(tipo_diseno = "Sin pesos") |>
  bind_rows(encuestar:::analizar_frecuencias(diseno = calibrated_design,pregunta = "voto2_pr") |>
              mutate(tipo_diseno = "Calibr. sexo + rango_edad + comuna")) |>
  mutate(tipo_diseno = factor(tipo_diseno, levels= c("Sin pesos",
                                                     "Calibr. sexo + rango_edad + comuna"))) |>
  encuestar:::graficar_barras(orden_respuestas = 1)+
  facet_wrap(~tipo_diseno)+
  tema_morant()


encuestar:::analizar_frecuencias_aspectos(diseno = diseno_sn_pesp,
                                          diccionario = diccionario |>
                                            rename(respuestas =  respuesta,llaves = llave),
                                          patron_pregunta = "conoce_per",
                                          aspectos = aspectos_conoce_per) |>
  mutate(tipo_diseno = "Sin pesos") |>
  filter(respuesta == "Sí") |>
  left_join()



#####################


diccionario |>
  filter(`tipo de pregunta` == "multiple") |> view()

error_muestral_maximo = function(diccionario,diseno,quitar_patron = NULL){
  aux <- diccionario %>% filter(tipo_pregunta == "multiple")
  if(!is.null(quitar_patron)) {
    quitar_patron <- paste(quitar_patron, collapse = "|")
    aux <- aux %>% filter(!grepl(quitar_patron, x = llaves))
  }

  aux <- aux %>% mutate(n = map_int(respuestas,~length(.x)))
  aux <- aux %>%
    pull(llaves) %>% map_df(~{
      # nas <- self$respuestas$base %>% summarise(any(is.na(c_across(.x)))) %>% pull(1)
      print(.x)
      survey::svymean(survey::make.formula(.x), design = diseno, na.rm = T) %>%
        tibble::as_tibble(rownames = "respuesta") %>%
        # rename(SE = 3) %>%
        mutate(pregunta = .x,
               # tiene_na = !!nas,
               # respuesta = stringr::str_replace(string = respuesta,
               #                                  pattern = as.character(.x),
               #                                  replacement = "")
        ) %>% select(respuesta, mean, SE)
    }) %>% mutate(SE = qnorm(.95)*SE)

  labels <- aux %>% summarise(inf = quantile(SE,.25,na.rm = T),
                              mid = quantile(SE,.5,na.rm = T),
                              sup = quantile(SE,.75,na.rm = T),
                              max = max(SE,na.rm = T)
  ) %>% mutate(iqr_min = inf-1.5*(sup-inf),
               iqr_max = sup + 1.5*(sup-inf)) %>%
    pivot_longer(everything(), names_to = "stat", values_to = "valor")

  a <- aux %>%
    ggplot() +
    geom_boxplot(aes(x = 0, y = SE)) +
    geom_label(data = labels, aes(x = 0, y = valor, label = scales::percent(valor)),
               hjust = 0, vjust = 0, nudge_x = .01) + labs(x = NULL) +
    scale_y_continuous(labels = scales::percent_format(1))

  b <- aux %>% filter(SE >= labels %>% filter(stat == "sup") %>% pull(valor)) %>%
    ggplot() + geom_col(aes(y = reorder(respuesta, SE), x = SE)) +
    geom_vline(xintercept = labels %>% filter(stat == "sup") %>% pull(valor))+
    labs(y = NULL)+
    scale_x_continuous(labels = scales::percent_format(1))

  print(a + b)
  return(aux)
}

bd_respuestas_efectivas |>
  select(cualidades_valora_candidato_O1,cualidades_valora_candidato_O2,cualidades_valora_candidato_O3) |>
  naniar::vis_miss()



error_muestral_maximo(diccionario = diccionario |>
                        janitor::clean_names() |>
                        rename(respuestas =  respuesta,llaves = llave,tipo_pregunta =tipo_de_pregunta ),
                      diseno = calibrated_design,
                      quitar_patron = c("opinion_","calif_","razon_no_trabajo","intentos","vivienda") )


error_muestral_maximo(diccionario = diccionario |>
                        janitor::clean_names() |>
                        rename(respuestas =  respuesta,llaves = llave,tipo_pregunta =tipo_de_pregunta ),
                      diseno = diseno_sn_pesp,
                      quitar_patron = c("opinion_","calif_","razon_no_trabajo","intentos","vivienda") )









