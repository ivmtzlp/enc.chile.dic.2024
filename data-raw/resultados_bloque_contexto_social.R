
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

bd_problema_chile |>
graficar_lollipops(width_cats = 35) +
  scale_color_manual(values = colores_problema) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_medios_com_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))
