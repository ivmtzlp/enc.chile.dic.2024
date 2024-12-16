
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(officer)

# devtools::load_all()

# Resultados ----------------------------------------------------------------------------------

g_interes_politica <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(interes_politica) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = interes_politica) |>
  graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  labs(caption = "¿Qué tan interesado está en temas de política?")

g_interes_eleccion_mun_24 <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(interes_eleccion_mun_24) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = interes_eleccion_mun_24) |>
  graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  labs(caption = "¿Cuán interesado estuvo usted en la elección municipal de octubre de 2024?")

g_participacion_pr_21 <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(participacion_pr_21) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = participacion_pr_21) |>
  graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  labs(caption = "¿Usted votó en las elecciones a la Presidencia de Chile del 2021?")

g_participacion_mun_24 <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(participacion_mun_24) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = participacion_mun_24) |>
  graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  labs(caption = "¿Usted votó en las pasadas elecciones municipales de octubre?")

g_conocimiento <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(SbjNum, starts_with("conoce_per_")) |>
  tidyr::pivot_longer(cols = !SbjNum,
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  group_by(pregunta) |>
  count(respuesta) |>
  mutate(pct = n/sum(n)) |>
  filter(respuesta == "Sí") |>
  ungroup() |>
  arrange(desc(pct)) |>
  select(!respuesta) |>
  rename(respuesta = pregunta,
         media = pct) |>
  graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16)) +
  labs(caption = "¿Usted conoce o ha oído hablar de...?") +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

g_voto_pr <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(voto_pr) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = voto_pr) |>
  graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  labs(caption = "Si las elecciones presidenciales fueran el próximo domingo, ¿por quién votarías?")

g_voto_pr_otro <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(voto2_pr) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = voto2_pr) |>
  graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16)) +
  labs(caption = "Si ese candidato no participara en las elecciones, ¿por cuál otro candidato votaría?") +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

# Exportar ------------------------------------------------------------------------------------

path_entregable <-
  formato_archivo(nombre = "./data-raw/pres_avances",
                  extension = "pptx",
                  tolerancia = 60)

dia <- lubridate::today() |> lubridate::day()

pptx <-
  read_pptx(path = "./data-raw/plantilla_general_09_12_24.pptx")

add_slide(pptx, layout = "gerencia_portada", master = "gerencia") %>%
  ph_with(value = 'Encuesta Nacional',
          location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = 'Chile',
          location = ph_location_label(ph_label = "subtitulo")) |>
  ph_with(value = paste0('Del 6 al ',dia,' de diciembre del 2024'),
          location = ph_location_label(ph_label = "periodo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_interes_politica,
          location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_interes_eleccion_mun_24,
          location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_participacion_pr_21,
          location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_participacion_mun_24,
          location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_conocimiento,
          location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_voto_pr,
          location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_voto_pr_otro,
          location = ph_location_label(ph_label = "imagen_principal"))

print(pptx, path_entregable)
