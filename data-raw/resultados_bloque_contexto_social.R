
# Bloque Contexto Social ----------------------------------------------------------------------

source(file = "./data-raw/parametros_bloque_contexto_social.R")

# pREGUNTA RANDOM
bd_temas <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(temas) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = temas)

g_temas <-
  bd_temas |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02) +
  scale_fill_manual(values = colores_temas) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_temas_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

# medios de comunicacion
bd_medios_com <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  count(medios_com) |>
  na.omit() |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = medios_com,
         pct = media)

g_medios_com <-
  bd_medios_com |>
  graficar_lollipops(width_cats = 35) +
  scale_color_manual(values = colores_medios_com) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_medios_com_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

# Redes sociales que utiliza
bd_utiliza <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(contains("utiliza")) |>
  tidyr::pivot_longer(cols = everything(),
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  group_by(pregunta) |>
  count(respuesta) |>
  mutate(pct = n/sum(n, na.rm = TRUE)) |>
  ungroup() |>
  filter(respuesta == "Sí") |>
  left_join(diccionario |>
              distinct(llave, tema),
            by = c("pregunta" = "llave")) |>
  transmute(respuesta = tema,
            media = pct)

g_utiliza <-
  bd_utiliza |>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.03) +
  scale_fill_manual(values = colores_utiliza) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = paste0(p_utiliza_tit,
                        "\n",
                        "El resultado corresponde a los que contestaron que Sí")) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

# Problemas de Chile
bd_problema_chile <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(SbjNum, contains("problema_chile_O")) |>
  tidyr::pivot_longer(cols = !SbjNum,
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  na.omit() |>
  count(respuesta) |>
  mutate(pct = n/nrow(bd_respuestas_efectivas))

g_problema_chile <-
  bd_problema_chile |>
  graficar_lollipops(width_cats = 35) +
  scale_color_manual(values = colores_problema) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_medios_com_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

# Calificaciones gobierno Boric

# Delincuencia
resultados_cali_delincuencia <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_delincuencia")

g_cali_delincuencia <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_delincuencia) |>
  ggplot(aes(x = as.numeric(cali_delincuencia))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_delincuencia$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_delincuencia$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_delincuencia$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" delincuencia"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_delincuencia$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))


bd_respuestas_efectivas |>
  as_tibble() |>
  select(starts_with("cali_")) |>
  glimpse()

resultados_cali_educacion <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_educacion")

g_cali_educacion <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_educacion) |>
  ggplot(aes(x = as.numeric(cali_educacion))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_educacion$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_educacion$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_educacion$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" educación"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_educacion$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

resultados_cali_salud <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_salud")

g_cali_salud <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_salud) |>
  ggplot(aes(x = as.numeric(cali_salud))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_salud$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_salud$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_salud$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" salud"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_salud$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

resultados_cali_empleo <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_empleo")

g_cali_empleo <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(!!rlang::sym(variable)) |>
  ggplot(aes(x = as.numeric(!!rlang::sym(variable)))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_empleo$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_empleo$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_empleo$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" empleo"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_empleo$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

resultados_cali_pensiones <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_pensiones")

g_cali_pensiones <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(!!rlang::sym(variable)) |>
  ggplot(aes(x = as.numeric(!!rlang::sym(variable)))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_pensiones$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_pensiones$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_pensiones$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" pensiones"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_pensiones$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

resultados_cali_ambiente <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_ambiente")

g_cali_ambiente <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(!!rlang::sym(variable)) |>
  ggplot(aes(x = as.numeric(!!rlang::sym(variable)))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_ambiente$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_ambiente$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_ambiente$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" ambiente"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_ambiente$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

resultados_cali_inmigracion <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_inmigracion")

g_cali_inmigracion <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(!!rlang::sym(variable)) |>
  ggplot(aes(x = as.numeric(!!rlang::sym(variable)))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_inmigracion$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_inmigracion$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_inmigracion$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" inmigración"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_inmigracion$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

resultados_cali_derechosmujer <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_derechosmujer")

g_cali_derechosmujer <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_derechosmujer) |>
  ggplot(aes(x = as.numeric(cali_derechosmujer))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_derechosmujer$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_derechosmujer$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_derechosmujer$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" inmigración"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_derechosmujer$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

resultados_cali_economia <-
  calcular_resultados_calificacion(bd_entrevistas_efectivas = bd_respuestas_efectivas,
                                   variable = "cali_economia")

g_cali_economia <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(cali_economia) |>
  ggplot(aes(x = as.numeric(cali_economia))) +
  geom_density(linewidth = 1.0, fill = "#C8E5F9", color = "#2297E6", alpha = .8, linewidth = 1.5) +
  geom_vline(xintercept = resultados_cali_economia$media, color = "#CF6076", linetype = "dashed", size = 2) +
  annotate("text",
           x = resultados_cali_economia$media + 0.5, y = .2,
           label = paste0("Promedio: ", round(resultados_cali_economia$media, 2)),
           size = 6) +
  scale_x_continuous(breaks = 1:7, labels = 1:7,
                     limits = c(1, 7)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = paste0(p_calificacion_gobierno,
                        stringr::str_to_title(" inmigración"),
                        "\n",
                        "Porcentage de Ns\\Nc: ",
                        scales::percent(resultados_cali_economia$pct_nsnc, accuracy = 1.))) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

