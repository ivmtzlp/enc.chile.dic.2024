library(tidyverse)

diccionario |> view()

dir_bd_tot<- "H:/Shared drives/Morant Consultores/Clientes/MarcoEnriquezOminami_Chile/Encuesta/Insumos/Personas Censal/Personas/Microdato_Censo2017-Personas.csv"

dir_bd_micro <- "H:/Shared drives/Morant Consultores/Clientes/MarcoEnriquezOminami_Chile/Encuesta/Insumos/Personas Censal/"

censo_total <- read.csv(dir_bd_tot,sep = ';')

comunas_micro <- read.csv(paste0(dir_bd_micro,'etiquetas_persona_comuna_16r.csv'),sep = ';')


censo_total |>
  select(P17) |>
  distinct()

names(censo_total)


# Vive en esta comuna actualmente
censo_total |>
  as_tibble() |>
  select(REGION:COMUNA,P08,P09,P10) |>
  rename(vivienda_comuna=P10,sexo=P08, edad=P09) |>
  left_join(read.csv(paste0(dir_bd_micro,'etiquetas_persona_p10.csv'),sep = ';') |>
              as_tibble()|>
              rename(vivienda_comuna=valor,vivienda_comuna_nom = glosa),
            by = 'vivienda_comuna') |>
  mutate(sexo_nom = case_match(sexo,
                               1~"Hombre",
                               2~"Mujer",
                               .default = NA)) |> #distinct(vivienda_comuna_nom)
  mutate(vivienda_comuna_nom = case_match(vivienda_comuna_nom,
                                          c("En otro país","En otra comuna")~"No",
                                          "Missing"~NA,
                                          .default = vivienda_comuna_nom)) |>
  left_join(comunas_micro |>
              rename(COMUNA=valor,NOM_COMUNA = glosa),
            by = 'COMUNA') |>
  count(REGION, PROVINCIA, COMUNA,NOM_COMUNA ,sexo_nom,edad, vivienda_comuna_nom,name = "poblacion") |>
write.csv('./data-raw/insumos_calibracion/poblacion_censo_vivienda_comuna_chile.csv')




# Asiste a la educacion actualmente
censo_total |>
  as_tibble() |>
  select(REGION:COMUNA,P08,P09,P13) |>
  rename(asiste_educacion=P13,sexo=P08, edad=P09) |>
  left_join(read.csv(paste0(dir_bd_micro,'etiquetas_persona_P13.csv'),sep = ';') |>
              as_tibble()|>
              rename(asiste_educacion=valor,asiste_educacion_nom = glosa),
            by = 'asiste_educacion') |> #distinct(asiste_educacion_nom)
  mutate(sexo_nom = case_match(sexo,
                               1~"Hombre",
                               2~"Mujer",
                               .default = NA)) |>
  mutate(asiste_educacion_nom = case_match(asiste_educacion_nom,
                                          "Missing"~NA,
                                          .default = asiste_educacion_nom)) |> #distinct(asiste_educacion_nom)
  left_join(comunas_micro |>
              rename(COMUNA=valor,NOM_COMUNA = glosa),
            by = 'COMUNA') |>
  count(REGION, PROVINCIA, COMUNA,NOM_COMUNA ,sexo_nom,edad, asiste_educacion_nom,name = "poblacion") |>
  write.csv('./data-raw/insumos_calibracion/poblacion_censo_asiste_educacion_chile.csv')






# grado_curso_aprobado
censo_total |>
  as_tibble() |>
  select(REGION:COMUNA,P08,P09,P14) |>
  rename(grado_curso_aprobado=P14,sexo=P08, edad=P09) |>
  left_join(read.csv(paste0(dir_bd_micro,'etiquetas_persona_p14.csv'),sep = ';') |>
              as_tibble()|>
              rename(grado_curso_aprobado=valor,grado_curso_aprobado_nom = glosa),
            by = 'grado_curso_aprobado') |>
  mutate(sexo_nom = case_match(sexo,
                               1~"Hombre",
                               2~"Mujer",
                               .default = NA)) |> #distinct(grado_curso_aprobado_nom)
  mutate(grado_curso_aprobado_nom = case_match(grado_curso_aprobado_nom,
                                          c("No aplica")~NA,
                                          "Missing"~NA,
                                          .default = grado_curso_aprobado_nom)) |> #distinct(grado_curso_aprobado_nom)
  left_join(comunas_micro |>
              rename(COMUNA=valor,NOM_COMUNA = glosa),
            by = 'COMUNA') |>
  count(REGION, PROVINCIA, COMUNA,NOM_COMUNA ,sexo_nom,edad, grado_curso_aprobado_nom,name = "poblacion") |>
  write.csv('./data-raw/insumos_calibracion/poblacion_censo_grado_curso_aprobado_chile.csv')



#curso aprobado actualmente
censo_total |>
  as_tibble() |>
  select(REGION:COMUNA,P08,P09,P15) |>
  rename(curso_aprobado=P15,sexo=P08, edad=P09) |>
  left_join(read.csv(paste0(dir_bd_micro,'etiquetas_persona_P15.csv'),sep = ';') |>
              as_tibble()|>
              rename(curso_aprobado=valor,curso_aprobado_nom = glosa),
            by = 'curso_aprobado') |>
  mutate(sexo_nom = case_match(sexo,
                               1~"Hombre",
                               2~"Mujer",
                               .default = NA)) |> #distinct(curso_aprobado_nom)
  mutate(curso_aprobado_nom = case_match(curso_aprobado_nom,
                                         c("Kínder","Prekínder","Sala cuna o jardín infantil")~"Preescolar",

                                         c("Especial o diferencial")~"Especial o diferencial",

                                         c("Primaria o preparatorio (sistema antiguo)",
                                           "Educación básica") ~"Básica o Primaria",

                                         c("Científico-humanista","Técnica comercial, industrial/normalista (sistema antiguo)",
                                           "Técnica profesional","Humanidades (sistema antiguo)") ~"Media o Secundaria",

                                         c("Profesional (4 o más años)","Técnico superior (1-3 años)",
                                           "Doctorado","Magíster") ~"Superior",

                                         c("No aplica")~NA,
                                         "Missing"~NA,
                                          .default = curso_aprobado_nom)) |># distinct(curso_aprobado_nom)
  left_join(comunas_micro |>
              rename(COMUNA=valor,NOM_COMUNA = glosa),
            by = 'COMUNA') |>
  count(REGION, PROVINCIA, COMUNA,NOM_COMUNA ,sexo_nom,edad, curso_aprobado_nom,name = "poblacion") |>
  write.csv('./data-raw/insumos_calibracion/poblacion_censo_curso_aprobado_chile.csv')


# perteneciente a grupo indigena
censo_total |>
  as_tibble() |>
  select(REGION:COMUNA,P08,P09,P16) |>
  rename(perteneciente_pueblo_indigena=P16,sexo=P08, edad=P09) |>
  left_join(read.csv(paste0(dir_bd_micro,'etiquetas_persona_p16.csv'),sep = ';') |>
              as_tibble()|>
              rename(perteneciente_pueblo_indigena=valor,perteneciente_pueblo_indigena_nom = glosa),
            by = 'perteneciente_pueblo_indigena') |>
  mutate(sexo_nom = case_match(sexo,
                               1~"Hombre",
                               2~"Mujer",
                               .default = NA)) |> #distinct(perteneciente_pueblo_indigena_nom)
  mutate(perteneciente_pueblo_indigena_nom = case_match(perteneciente_pueblo_indigena_nom,

                                               "Missing"~NA,
                                               .default = perteneciente_pueblo_indigena_nom)) |> #distinct(perteneciente_pueblo_indigena_nom)
  left_join(comunas_micro |>
              rename(COMUNA=valor,NOM_COMUNA = glosa),
            by = 'COMUNA') |>
  count(REGION, PROVINCIA, COMUNA,NOM_COMUNA ,sexo_nom,edad, perteneciente_pueblo_indigena_nom,name = "poblacion") |>
  write.csv('./data-raw/insumos_calibracion/poblacion_censo_perteneciente_pueblo_indigena_chile.csv')




# perteneciente a grupo indigena
censo_total |>
  as_tibble() |>
  select(REGION:COMUNA,P08,P09,P17) |>
  rename(semana_pasada_trabajo=P17,sexo=P08, edad=P09) |>
  left_join(read.csv(paste0(dir_bd_micro,'etiquetas_persona_P17.csv'),sep = ';') |>
              as_tibble()|>
              rename(semana_pasada_trabajo_raz=valor,semana_pasada_trabajo_nom = glosa),
            by = 'semana_pasada_trabajo') |>
  mutate(sexo_nom = case_match(sexo,
                               1~"Hombre",
                               2~"Mujer",
                               .default = NA)) |> #distinct(semana_pasada_trabajo_nom)
  mutate(semana_pasada_trabajo_raz = case_match(semana_pasada_trabajo_raz,
                                                        "Missing"~NA,
                                                        .default = semana_pasada_trabajo_nom)) |> #distinct(semana_pasada_trabajo_nom)
  left_join(comunas_micro |>
              rename(COMUNA=valor,NOM_COMUNA = glosa),
            by = 'COMUNA') |>
  count(REGION, PROVINCIA, COMUNA,NOM_COMUNA ,sexo_nom,edad, semana_pasada_trabajo_nom,name = "poblacion") |>
  write.csv('./data-raw/insumos_calibracion/poblacion_censo_semana_pasada_trabajo_chile.csv')














