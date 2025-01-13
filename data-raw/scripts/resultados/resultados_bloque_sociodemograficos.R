
#diccionario |> View()
source(file = './data-raw/scripts/parametros/parametros_bloque_sociodemograficos.R')

#Rango de edad
rango_edad_graf <-
bd_respuestas_efectivas |>
  group_by(rango_edad =  case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                   edad >= 30 & edad <= 39 ~ "30-39",
                                   edad >= 40 & edad <= 49 ~ "40-49",
                                   edad >= 50 & edad <= 59 ~ "50-59",
                                   edad >= 60 & edad <= 64 ~ "60-64",
                                   edad >= 65 ~ "65+",
                                   T ~ NA)
  )|>
  count(rango_edad,wt = pesos) |>
  ungroup() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = rango_edad) |>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = c("65+", "60-64","50-59","40-49","30-39","18-29") )+
  scale_fill_manual(values = rep(color_general,7))+
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = ""  )+
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


# Nivel de educacion
bd_educacion_jefe_hogar <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(educacion_jefe_hogar,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = educacion_jefe_hogar)

g_educacion_jefe_hogar <-
  bd_educacion_jefe_hogar |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(orden_educacion_jefe_hogar)) +
  scale_fill_manual(values = colores_educacion_jefe_hogar) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_educacion_jefe_hogar_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))


# ocupacion del jefe
bd_ocupacion_jefe_hogar <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(ocupacion_jefe_hogar,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = ocupacion_jefe_hogar)

g_ocupacion_jefe_hogar <-
  bd_ocupacion_jefe_hogar |>
  graficar_barras(salto = 60,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = orden_ocupacion_jefe_hogar) +
  scale_fill_manual(values = colores_ocupacion_jefe_hogar) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_ocupacion_jefe_hogar_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 10),
        plot.caption = element_text(size = 12))


# Numero de miembros en la familia
bd_personas_viven_hogar <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(personas_viven_hogar,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = personas_viven_hogar,
         pct = media)

g_personas_viven_hogar <-
  bd_personas_viven_hogar |>
  graficar_lollipops(width_cats = 35,
                     orden = orden_personas_viven_hogar) +
  scale_color_manual(values = colores_personas_viven_hogar) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_personas_viven_hogar_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 12))



# Ingreso hogar
bd_ingreso_mensual_hogar <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(ingreso_mensual_hogar,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = ingreso_mensual_hogar)

g_ingreso_mensual_hogar <-
  bd_ingreso_mensual_hogar |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = orden_ingreso_mensual_hogar) +
  scale_fill_manual(values = colores_ingreso_mensual_hogar) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_ingreso_mensual_hogar_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))


# Vive en la comuna

bd_vivienda_comuna <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(vivienda_comuna,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = vivienda_comuna)

g_vivienda_comuna <-
  bd_vivienda_comuna |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = orden_vivienda_comuna) +
  scale_fill_manual(values = colores_vivienda_comuna) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  labs(caption = p_vivienda_comuna_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))


# Asistencia a la escuela
bd_asiste_educacion <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(asiste_educacion,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = asiste_educacion)

g_asiste_educacion <-
  bd_asiste_educacion |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = orden_asiste_educacion) +
  scale_fill_manual(values = colores_asiste_educacion) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  labs(caption = p_asiste_educacion_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))

# grado Curso aprobado

bd_grado_curso_aprobado <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(grado_curso_aprobado,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = grado_curso_aprobado,
         pct = media)

g_grado_curso_aprobado <-
  bd_grado_curso_aprobado |>
  graficar_lollipops(width_cats = 35,
                     orden = orden_grado_curso_aprobado) +
  scale_color_manual(values = colores_grado_curso_aprobado) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_grado_curso_aprobado_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        #axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 12))


# Curso aprobado
bd_curso_aprobado <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(curso_aprobado,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = curso_aprobado)

g_curso_aprobado <-
  bd_curso_aprobado |>
  graficar_barras(salto = 10,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = orden_curso_aprobado) +
  scale_fill_manual(values = colores_curso_aprobado) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_curso_aprobado_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 14),
        #axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))

#

#

# origen indigena
bd_perteneciente_pueblo_indigena<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(perteneciente_pueblo_indigena,pesos) |>
  count(perteneciente_pueblo_indigena,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=perteneciente_pueblo_indigena ) |>
  filter(respuesta == 'Sí')


p_perteneciente_pueblo_indigena_graf <-
  bd_perteneciente_pueblo_indigena |>
  graficar_gauge(color_principal =color_general,escala = c(0,1),size_text_pct = 12)+
  labs(title = p_perteneciente_pueblo_indigena_tit,
       caption =  "Entrevistados que contestaron que Sí")+
  theme(plot.title = element_text(size = 12))


# Trabaja actualmente
bd_semana_pasada_trabajo<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(semana_pasada_trabajo,pesos) |>
  count(semana_pasada_trabajo,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=semana_pasada_trabajo ) |>
  filter(respuesta == 'Trabajó')


p_semana_pasada_trabajo_graf <-
  bd_semana_pasada_trabajo |>
  graficar_gauge(color_principal =color_general,escala = c(0,1),size_text_pct = 12)+
  labs(title = p_semana_pasada_trabajo_tit,
       caption =  "Entrevistados que contestaron que Sí trabajó")+
  theme(plot.title = element_text(size = 12))

# Razon no trabajo
bd_razon_no_trabajo <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(razon_no_trabajo,wt = pesos) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = razon_no_trabajo)

g_razon_no_trabajo <-
  bd_razon_no_trabajo |>
  graficar_barras(salto = 23,
                  porcentajes_fuera = F,
                  text_size = 6,
                  desplazar_porcentajes = 0.02) +
  scale_fill_manual(values = colores_razon_no_trabajo) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_razon_no_trabajo_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 12))

