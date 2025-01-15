
path_insumos_censos <- c("H:/Shared drives/Morant Consultores/Clientes/MarcoEnriquezOminami_Chile/Encuesta/Bases de datos/insumos_calibracion/")


comunas_encuesta <- bd_respuestas_efectivas |>
  distinct(comuna) |>
  pull(comuna)


###############################################################################################

########################
#Comparacion sexo
########################


#bd_sexo_censo <- readxl::read_xlsx('./data-raw/poblacion_censo_chile.xlsx')

# bd_sexo_enc_sexo <-
#   bd_sexo_censo |>
#   mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
#                                                   "Latin-ASCII")) |>
#   filter(NOM_COMUNA %in% comunas_encuesta) |>
#   summarise(across(.cols = c(T_HOM:T_MUJ),.fns = sum)) |>
#   mutate(tipo = "a") |>
#   rename(Hombre = T_HOM, Mujer  = T_MUJ) |>
#   tidyr::pivot_longer(cols = -tipo,names_to = 'sexo',values_to = 'poblacion') |>
#   mutate(media = poblacion/sum(poblacion)) |>
#   left_join(bd_respuestas_efectivas |>
#               count(sexo,name = "poblacion_enc") |>
#               mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
#             by = "sexo"
#   ) |>
#   mutate(dif = media_enc- media )


censo_tot_comu_sexo_edad<-
  read.csv('./data-raw/poblacion_censo_sexo_edad__comuna_chile.csv') |>
  as_tibble()


bd_sexo_enc_censo <-
  censo_tot_comu_sexo_edad |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |> # distinct(NOM_COMUNA)
  filter(edad >=18) |>
  group_by(sexo_nom) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  rename(sexo = sexo_nom) |>
  left_join(bd_respuestas_efectivas |>
              count(sexo,name = "poblacion_enc") |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
            by = "sexo"
  ) |>
  mutate(dif = media_enc- media )





dif_encuesta_censo_sexo_graf <-
  bd_sexo_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = sexo,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .02, y = 2.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.02, y = 2.5, size = 4, colour = "red"
  )+
  tema_morant()


########################
#Comparacion edad
########################


bd_rango_edad_enc_censo <-
censo_tot_comu_sexo_edad |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |> # distinct(NOM_COMUNA)
  filter(edad >=18) |>
  group_by(rango_edad =  case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                   edad >= 30 & edad <= 39 ~ "30-39",
                                   edad >= 40 & edad <= 49 ~ "40-49",
                                   edad >= 50 & edad <= 59 ~ "50-59",
                                   edad >= 60 & edad <= 64 ~ "60-64",
                                   edad >= 65 ~ "65+",
                                   T ~ NA)
  )|>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  left_join(bd_respuestas_efectivas |>
              group_by(rango_edad =  case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                               edad >= 30 & edad <= 39 ~ "30-39",
                                               edad >= 40 & edad <= 49 ~ "40-49",
                                               edad >= 50 & edad <= 59 ~ "50-59",
                                               edad >= 60 & edad <= 64 ~ "60-64",
                                               edad >= 65 ~ "65+",
                                               T ~ NA)
              )|>
              count(rango_edad,name = "poblacion_enc") |>
              ungroup() |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)) ,
            #rename(respuesta = rango_edad),
            by = 'rango_edad') |>
  mutate(dif = media_enc- media )




dif_encuesta_censo_rango_edad_graf <-
  bd_rango_edad_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = rango_edad,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .052, y = 6.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.052, y = 6.5, size = 4, colour = "red"
  )+
  tema_morant()


#########################################################


bd_comuna_enc_censo <-
  censo_tot_comu_sexo_edad |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>  #distinct(NOM_COMUNA)
  filter(edad >= 18) |>
  left_join(bd_respuestas_efectivas |>
              distinct(comuna_mm,comuna),
            by = c("NOM_COMUNA" = "comuna")) |>
  group_by(comuna_mm) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  left_join(
    bd_respuestas_efectivas |>
      count(comuna_mm,name = "poblacion_enc") |>
      mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
    by = "comuna_mm"
  )|>
  mutate(dif = media_enc- media )



dif_comuna_enc_censo_graf <-
  bd_comuna_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = comuna_mm,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .052, y = 16.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.052, y = 16.5, size = 4, colour = "red"
  )+
  tema_morant()

##################################################
########################
#Comparacion Vive en esta comuna actualmente
########################

censo_tot_comu_vivienda_comuna_nom<-
  read.csv(paste0(path_insumos_censos,"poblacion_censo_vivienda_comuna_chile.csv")) |>
  as_tibble()



bd_vivienda_comuna_enc_censo <-
  censo_tot_comu_vivienda_comuna_nom |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>  #distinct(NOM_COMUNA)
  filter(edad >=18) |>
  filter(!is.na(vivienda_comuna_nom)) |>
  group_by(vivienda_comuna_nom) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  rename(vivienda_comuna = vivienda_comuna_nom) |>
  left_join(bd_respuestas_efectivas |>
              count(vivienda_comuna,name = "poblacion_enc") |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
            by = "vivienda_comuna"
  ) |>
  mutate(dif = media_enc- media )


dif_encuesta_censo_vivienda_comuna_graf <-
  bd_vivienda_comuna_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = vivienda_comuna,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .02, y = 3.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.02, y = 3.5, size = 4, colour = "red"
  )+
  tema_morant()



########################
#Comparacion Asiste a la educacion actualmente
########################


censo_tot_comu_asiste_educacion_nom<-
  read.csv(paste0(path_insumos_censos,"poblacion_censo_asiste_educacion_chile.csv")) |>
  as_tibble()



bd_asiste_educacion_enc_censo <-
  censo_tot_comu_asiste_educacion_nom |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>  #distinct(NOM_COMUNA)
  filter(edad >=18) |>
  filter(!is.na(asiste_educacion_nom)) |>
  group_by(asiste_educacion_nom) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  rename(asiste_educacion = asiste_educacion_nom) |>
  left_join(bd_respuestas_efectivas |>
              count(asiste_educacion,name = "poblacion_enc") |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
            by = "asiste_educacion"
  ) |>
  mutate(dif = media_enc- media )


dif_encuesta_censo_asiste_educacion_graf <-
  bd_asiste_educacion_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = asiste_educacion,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .02, y = 3.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.02, y = 3.5, size = 4, colour = "red"
  )+
  tema_morant()



########################
#Comparacion ultimo curso aprobado
########################


censo_tot_comu_curso_aprobado_nom<-
  read.csv(paste0(path_insumos_censos,"poblacion_censo_curso_aprobado_chile.csv")) |>
  as_tibble()



bd_curso_aprobado_enc_censo <-
  censo_tot_comu_curso_aprobado_nom |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>  #distinct(NOM_COMUNA)
  filter(edad >=18) |>
  filter(!is.na(curso_aprobado_nom)) |>
  group_by(curso_aprobado_nom) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  rename(curso_aprobado = curso_aprobado_nom) |>
  left_join(bd_respuestas_efectivas |>
              count(curso_aprobado,name = "poblacion_enc") |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
            by = "curso_aprobado"
  ) |>
  mutate(dif = media_enc- media )


dif_encuesta_censo_curso_aprobado_graf <-
  bd_curso_aprobado_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = curso_aprobado,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .02, y = 5.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.02, y = 5.5, size = 4, colour = "red"
  )+
  tema_morant()



########################
#Comparacion perteneciente a grupo indigena
########################


censo_tot_comu_curso_aprobado_nom<-
  read.csv(paste0(path_insumos_censos,"poblacion_censo_curso_aprobado_chile.csv")) |>
  as_tibble()



bd_curso_aprobado_enc_censo <-
  censo_tot_comu_curso_aprobado_nom |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>  #distinct(NOM_COMUNA)
  filter(edad >=18) |>
  filter(!is.na(curso_aprobado_nom)) |>
  group_by(curso_aprobado_nom) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  rename(curso_aprobado = curso_aprobado_nom) |>
  left_join(bd_respuestas_efectivas |>
              count(curso_aprobado,name = "poblacion_enc") |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
            by = "curso_aprobado"
  ) |>
  mutate(dif = media_enc- media )


dif_encuesta_censo_curso_aprobado_graf <-
  bd_curso_aprobado_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = curso_aprobado,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .02, y = 5.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.02, y = 5.5, size = 4, colour = "red"
  )+
  tema_morant()


########################
#Comparacion perteneciente a grupo indigena
########################


censo_tot_comu_perteneciente_pueblo_indigena_nom<-
  read.csv(paste0(path_insumos_censos,"poblacion_censo_perteneciente_pueblo_indigena_chile.csv")) |>
  as_tibble()



bd_perteneciente_pueblo_indigena_enc_censo <-
  censo_tot_comu_perteneciente_pueblo_indigena_nom |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>  #distinct(NOM_COMUNA)
  filter(edad >=18) |>
  filter(!is.na(perteneciente_pueblo_indigena_nom)) |>
  group_by(perteneciente_pueblo_indigena_nom) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  rename(perteneciente_pueblo_indigena = perteneciente_pueblo_indigena_nom) |>
  left_join(bd_respuestas_efectivas |>
              count(perteneciente_pueblo_indigena,name = "poblacion_enc") |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
            by = "perteneciente_pueblo_indigena"
  ) |>
  mutate(dif = media_enc- media )


dif_encuesta_censo_perteneciente_pueblo_indigena_graf <-
  bd_perteneciente_pueblo_indigena_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = perteneciente_pueblo_indigena,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .01, y = 2.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.01, y = 2.5, size = 4, colour = "red"
  )+
  tema_morant()


########################
#Comparacion trabajo la semana pasada
########################


censo_tot_comu_semana_pasada_trabajo_nom<-
  read.csv(paste0(path_insumos_censos,"poblacion_censo_semana_pasada_trabajo_chile.csv")) |>
  as_tibble()



bd_semana_pasada_trabajo_enc_censo <-
  censo_tot_comu_semana_pasada_trabajo_nom |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>  #distinct(NOM_COMUNA)
  filter(edad >=18) |>
  filter(!is.na(semana_pasada_trabajo_nom)) |>
  group_by(semana_pasada_trabajo_nom) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  rename(semana_pasada_trabajo = semana_pasada_trabajo_nom) |>
  left_join(bd_respuestas_efectivas |>
              count(semana_pasada_trabajo,name = "poblacion_enc") |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)),
            by = "semana_pasada_trabajo"
  ) |>
  mutate(dif = media_enc- media )


dif_encuesta_censo_semana_pasada_trabajo_graf <-
  bd_semana_pasada_trabajo_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = semana_pasada_trabajo,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .02, y = 2.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.02, y = 2.5, size = 4, colour = "red"
  )+
  tema_morant()



########################
#Comparacion trabajo la semana pasada
########################


bd_razon_no_trabajo_enc_censo <-
  censo_tot_comu_semana_pasada_trabajo_nom |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  mutate(NOM_COMUNA =  stringr::str_to_upper(NOM_COMUNA)) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>  #distinct(NOM_COMUNA)
  filter(edad >=18) |>
  filter(!is.na(semana_pasada_trabajo_nom)) |>
  filter(semana_pasada_trabajo_nom == "No trabajó") |>
  group_by(semana_pasada_trabajo_raz) |>
  summarise(poblacion = sum(poblacion)) |>
  ungroup() |>
  mutate(media = poblacion/sum(poblacion)) |>
  rename(razon_no_trabajo = semana_pasada_trabajo_raz) |>
  mutate(razon_no_trabajo  = gsub("Tenía empleo pero estuvo de vacaciones, con licencia, en descanso laboral, etc.",
                  "Estuvo de vacaciones, en descanso laboral, etc",
                  razon_no_trabajo)) |>
  left_join(bd_respuestas_efectivas |>
              filter(semana_pasada_trabajo == "No trabajó") |>
              count(razon_no_trabajo,name = "poblacion_enc") |>
              mutate(media_enc = poblacion_enc/sum(poblacion_enc)) |>
              mutate(razon_no_trabajo = gsub("\\.","",razon_no_trabajo)),
            by = "razon_no_trabajo"
  ) |>
  mutate(dif = media_enc- media )


dif_encuesta_censo_razon_no_trabajo_graf <-
  bd_razon_no_trabajo_enc_censo |>
  mutate(tipo = "a") |>
  ggplot(aes(y = razon_no_trabajo,x = dif, grupo= tipo, colour = tipo) )+
  geom_line(show.legend = FALSE,
            linewidth = 1) +
  geom_point(size = 3)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_vline(xintercept = 0,linetype = 'dashed')+
  geom_text(aes(label = scales::percent(x = dif, accuracy = 1.1)),nudge_y = .1)+
  annotate(
    "text", label = "Sobre representado",
    x = .05, y = 6.5, size = 4, colour = "blue"
  )+
  annotate(
    "text", label = "Subrepresentado",
    x = -.05, y = 6.5, size = 4, colour = "red"
  )+
  tema_morant()



##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################


####################################################################################333
####################################################################################333
#Press
####################################################################################333
####################################################################################333
library(officer)

path_export <-
  encuestar:::formato_archivo(nombre = "./data-raw/press/comparacion_censo",
                              extension = "pptx",
                              tolerancia = 60)



pptx <-
  read_pptx(path = "./data-raw/plantilla_general_09_12_24.pptx")



add_slide(pptx, layout = "gerencia_portada", master = "gerencia") %>%
  ph_with(value = 'Encuesta Nacional',
          location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = 'Chile',
          location = ph_location_label(ph_label = "subtitulo")) |>
  ph_with(value = paste0('Comparación de población encuesta con INEGI'),
          location = ph_location_label(ph_label = "periodo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = dif_encuesta_censo_sexo_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  # ph_with(value = proyecto_fecha, location = ph_location_label(ph_label = "fecha")) |>
  ph_with(value = 'Relación por sexo',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = dif_encuesta_censo_rango_edad_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  # ph_with(value = proyecto_fecha, location = ph_location_label(ph_label = "fecha")) |>
  ph_with(value = 'Relación por rango de edad',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = dif_encuesta_censo_vivienda_comuna_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  # ph_with(value = proyecto_fecha, location = ph_location_label(ph_label = "fecha")) |>
  ph_with(value = 'Relación por la pregunta ¿Vive actualmente en esta comuna?',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = dif_encuesta_censo_asiste_educacion_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  # ph_with(value = proyecto_fecha, location = ph_location_label(ph_label = "fecha")) |>
  ph_with(value = 'Relación por la pregunta ¿Asiste actualmente a la educación formal?',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = dif_encuesta_censo_curso_aprobado_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  # ph_with(value = proyecto_fecha, location = ph_location_label(ph_label = "fecha")) |>
  ph_with(value = 'Relación por nivel de educación',
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = dif_encuesta_censo_perteneciente_pueblo_indigena_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  # ph_with(value = proyecto_fecha, location = ph_location_label(ph_label = "fecha")) |>
  ph_with(value = 'Relación por pertenencia a pueblos indigenas',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = dif_encuesta_censo_semana_pasada_trabajo_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  # ph_with(value = proyecto_fecha, location = ph_location_label(ph_label = "fecha")) |>
  ph_with(value = 'Relación por si trabajó la semana pasada',
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = dif_encuesta_censo_razon_no_trabajo_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  # ph_with(value = proyecto_fecha, location = ph_location_label(ph_label = "fecha")) |>
  ph_with(value = 'Relación por "razónpor la que no trabajó" ',
          location = ph_location_label(ph_label = "titulo"))


print(pptx, path_export)
