
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(officer)

# devtools::load_all()

load(file = "./data/diccionario.rda")
load(file = "./data/bd_comunas_regionMetropolitanaSantiago.rda")
load(file = "./data/bd_cuotas_comuna.rda")

# Resultados ----------------------------------------------------------------------------------

# INTERES EN POLITCA

p_interes_politica_tit <-
  "¿Qué tan interesado está en temas de política?"

g_interes_politica <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(interes_politica) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = interes_politica) |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(c("Muy interesado",
                                           "Interesado",
                                           "Neutral/Indiferente",
                                           "Muy poco interesado",
                                           "Nada interesado",
                                           "Ns/Nc"))) +
  scale_fill_manual(values = c("Muy interesado" = color_general,
                               "Interesado" = color_general,
                               "Neutral/Indiferente" = color_general,
                               "Muy poco interesado" = color_general,
                               "Nada interesado" = color_general,
                               "Ns/Nc" = color_nsnc)) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

p_interes_elecciones_mun_24_tit <-
  "¿Cuán interesado estuvo usted en la elección municipal de octubre de 2024?"

g_interes_eleccion_mun_24 <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(interes_eleccion_mun_24) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = interes_eleccion_mun_24) |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(c("Muy interesado",
                                           "Interesado",
                                           "Neutral/Indiferente",
                                           "Muy poco interesado",
                                           "Nada interesado",
                                           "Ns/Nc"))) +
  scale_fill_manual(values = c("Muy interesado" = color_general,
                               "Interesado" = color_general,
                               "Neutral/Indiferente" = color_general,
                               "Muy poco interesado" = color_general,
                               "Nada interesado" = color_general,
                               "Ns/Nc" = color_nsnc)) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

g_participacion_pr_21 <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(participacion_pr_21) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = participacion_pr_21) |>
  filter(respuesta == "Sí") |>
  graficar_gauge(color_principal = color_general,
                 escala = c(0, 1),
                 size_text_pct = 12) +
  labs(title = "¿Usted votó en las elecciones a la\nPresidencia de Chile del 2021?",
       caption = "Entrevistados que contestaron que Sí") +
  theme(plot.caption = element_text(size = 14))

g_participacion_mun_24 <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(participacion_mun_24) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = participacion_mun_24) |>
  filter(respuesta == "Sí") |>
  graficar_gauge(color_principal = color_general,
                 escala = c(0, 1),
                 size_text_pct = 12) +
  labs(title = "¿Usted votó en las pasadas elecciones\nmunicipales de octubre?",
       caption = "Entrevistados que contestaron que Sí") +
  theme(plot.caption = element_text(size = 14))

bd_conocimiento <-
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
  left_join(diccionario |>
              distinct(llave, tema),
            by = c("pregunta" = "llave")) |>
  transmute(respuesta = tema,
            media = pct)

p_conocimiento_tit <-
  "¿Usted conoce o ha oído hablar de...?"

g_conocimiento <-
  bd_conocimiento |>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.05) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16)) +
  scale_fill_manual(values = c("Michelle Bachellete" = color_nsnc,
                               "Evelyn Mathei" = color_nsnc,
                               "José Antonio Kast" = color_nsnc,
                               "Carolina Tohá" = color_nsnc,
                               "Marco Enríquez-Ominami" = color_ominami,
                               "Franco Parisi" = color_nsnc,
                               "Tomás Vodanovic" = color_nsnc,
                               "Johannes Kaiser" = color_nsnc,
                               "Gonzalo Winter" = color_nsnc)) +
  labs(caption = "El porcentaje corresponde a los que contestaron 'Sí lo conoce'") +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 14))

p_voto_pr_tit <-
  "Si las elecciones presidenciales fueran el próximo domingo, ¿por quién votarías?"

g_voto_pr <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(voto_pr) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = voto_pr) |>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.01) +
  tema_morant() +
  scale_fill_manual(values = c("Michelle Bachellete" = color_nsnc,
                               "Evelyn Mathei" = color_nsnc,
                               "José Antonio Kast" = color_nsnc,
                               "Carolina Tohá" = color_nsnc,
                               "Marco Enríquez-Ominami" = color_ominami,
                               "Franco Parisi" = color_nsnc,
                               "Tomás Vodanovic" = color_nsnc,
                               "Johannes Kaiser" = color_nsnc,
                               "Gonzalo Winter" = color_nsnc)) +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

p_voto_pr_otro_tit <-
  "Si ese candidato no participara en las elecciones, ¿por cuál otro candidato votaría?"

g_voto_pr_otro <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(voto2_pr) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = voto2_pr) |>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.01) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16)) +
  scale_fill_manual(values = c("Michelle Bachellete" = color_nsnc,
                               "Evelyn Mathei" = color_nsnc,
                               "José Antonio Kast" = color_nsnc,
                               "Carolina Tohá" = color_nsnc,
                               "Marco Enríquez-Ominami" = color_ominami,
                               "Franco Parisi" = color_nsnc,
                               "Tomás Vodanovic" = color_nsnc,
                               "Johannes Kaiser" = color_nsnc,
                               "Gonzalo Winter" = color_nsnc)) +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

tot_efectivas <-
  bd_respuestas_efectivas %>%
  as_tibble |>
  count(comuna_mm, sort = TRUE, name = "Efectivas") |>
  tidyr::complete(comuna_mm = unique(bd_cuotas_comuna$comuna),
                  fill = list(Efectivas = 0)) |>
  left_join(bd_cuotas_comuna, by = c("comuna_mm" = "comuna")) |>
  mutate(Faltantes = cuota - Efectivas,
         pct = Efectivas/cuota) |>
  arrange(desc(pct))

tot_efectivas_resumen <-
  tot_efectivas |>
  summarise(across(.cols = c("Efectivas", "cuota", "Faltantes"),
                   .fns = ~ sum(.x, na.rm = TRUE))) |>
  mutate(pct = Efectivas/cuota,
         comuna_mm = "Total")

tot_efectivas_flex <-
  tot_efectivas |>
  bind_rows(tot_efectivas_resumen) |>
  mutate(comuna_mm = stringr::str_to_title(string = comuna_mm),
         pct = scales::percent(x = pct, accuracy = 1.)) |>
  rename(Comuna = comuna_mm,
         'Entrevistas efectivas' = Efectivas,
         'Cuota' = cuota,
         'Faltantes' = Faltantes,
         '% de avance' = pct) |>
  flextable::flextable(cwidth = 3, cheight = 0.7) |>
  flextable::padding(padding.top = 0, padding.bottom = 0, part = "body") |>
  flextable::autofit() |>
  flextable::border_outer(part = "header", border = officer::fp_border(color = "black", width = 1)) |>
  flextable::border_inner_v(border = officer::fp_border(color = "black", width = 1), part = "header") |>
  flextable::align(i = 1, j = 2, align = "center", part = "header") |>
  flextable::align(align = "center", part = "body") |>
  flextable::align(j = 1, align = "left", part = "body") |>
  flextable::border_inner_h(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
  flextable::border_inner_v(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
  flextable::border_outer(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
  flextable::fontsize(size = 16, part = "header") |>
  flextable::fontsize(size = 14, part = "body") |>
  flextable::font(fontname = "Poppins", part = "all") |>
  flextable::bold(part = "header", bold = TRUE) |>
  flextable::padding(part = "body", padding.bottom = 0, padding.top = 0)

g_rechazo <-
  calcular_tasa_rechazo(bd_respuestas_efectivas = bd_respuestas_efectivas |>
                          as_tibble()) |>
  as_tibble() |>
  transmute(media = rechazo,
            respuesta = "Sí") |>
  encuestar:::graficar_gauge(color_principal = color_general,
                             escala = c(0, 1),
                             size_text_pct = 12) +
  labs(title = stringr::str_wrap(string = "La tasa se calcula como los intentos de levantamiento rechazados entre el total intentos de levantamiento",
                                 width = 55),
       caption = stringr::str_wrap(string = "Por ejemplo, un rechazo del 75% nos dice que de 4 intentos, 3 fueron rechazados y 1 fue efectivo",
                                   width = 55))



bd_temas_interes_politica <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(temas,interes_politica,pesos) |>
  filter(!is.na(temas)) |>
  filter(!is.na(interes_politica)) |>
  count(temas,interes_politica,wt = pesos) |>
  group_by(temas) |>
  mutate(media = n /sum(n)) |>
  ungroup() |>
  tidyr::complete(temas,interes_politica,fill = list(n = 0, media =0)) |>

  mutate(media = scales::percent(x = media,accuracy=1.0))


orden_temas_interes_politica <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(temas,pesos) |>
  count(temas,wt = pesos) |>
  mutate(media = n/sum(n)) |>
  arrange(desc(media)) |>
  mutate(temas = as.character(temas)) |>
  select(temas, media)

colores_temas_interes_2 <-
  orden_temas_interes_politica |>
  select(temas) |>
  asignar_colores()


colores_temas_interes_2["Política"] <- color_ominami
colores_temas_interes_2<- colores_temas_interes_2[!names(colores_temas_interes_2) %in% c("Salud","Deportes", "Películas")]



bd_temas_interes_politica <-
  bd_temas_interes_politica|>
  mutate(interes_politica = factor(interes_politica,
                                   levels = c("Muy interesado","Interesado" ,"Neutral/Indiferente","Muy poco interesado","Nada interesado","Ns/Nc")  )) |>
  arrange(interes_politica) |>
  tidyr::pivot_wider(id_cols = temas,
                     names_from = interes_politica,
                     values_from = media) |>
  left_join(orden_temas_interes_politica,
            by = "temas") |>
  arrange(desc(media)) |>
  select(!media) |>
  rename(respuesta = temas)


temas_interes_politica_tbl <-
  bd_temas_interes_politica|>
  encuestar:::formatear_tabla_votoCruzado(
    var1 = "respuesta",
    var2 = "",
    filtro_var2 = NULL,
    etiquetas = c("Temas de interés","Nivel de interés \nen la política"),
    colores_var1 = colores_temas_interes_2,
    colores_var2 = rep("white",7),
    size_text_header = 18,
    size_text_body = 14,
    salto = 20
  )|>
  flextable::color(color = "black", part = "header", i = 2) |>
  flextable::bg(i = ~ respuesta == 'Política', bg=color_ominami,part="body") |>
  flextable::color(i = ~ respuesta == 'Política', color='white',part="body") |>
  flextable::autofit()



# Exportar ------------------------------------------------------------------------------------

path_entregable <-
  formato_archivo(nombre = "./data-raw/press/pres_avances",
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
  ph_with(value = g_rechazo,
          location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = "Tasa de rechazo",
          location = ph_location_label(ph_label = "titulo"))


add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = tot_efectivas_flex,
          location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = "Progreso",
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_interes_politica,
          location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = p_interes_politica_tit,
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_interes_eleccion_mun_24,
          location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = p_interes_elecciones_mun_24_tit,
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_dos_graficas_equitativas", master = "gerencia") %>%
  ph_with(value = g_participacion_pr_21,
          location = ph_location_label(ph_label = "grafica_uno")) |>
  ph_with(value = g_participacion_mun_24,
          location = ph_location_label(ph_label = "grafica_dos")) |>
  ph_with(value = "Participación Ciudadana",
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_conocimiento,
          location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = p_conocimiento_tit,
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_voto_pr,
          location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = p_voto_pr_tit,
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = g_voto_pr_otro,
          location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = p_voto_pr_otro_tit,
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "gerencia_grafica_unica", master = "gerencia") %>%
  ph_with(value = temas_interes_politica_tbl,
          location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = "Temas de interés vs nivel de interés en la política",
          location = ph_location_label(ph_label = "titulo"))

print(pptx, path_entregable)
