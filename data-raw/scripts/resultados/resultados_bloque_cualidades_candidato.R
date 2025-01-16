# bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(participacion_primarias)|>
#   naniar::vis_miss()


source('./data-raw/scripts/parametros/parametros_bloque_cualidades_candidato.R')

#######################################333

# Cualidad mas valorada

bd_cualidades_valora_candidato <-
  bd_respuestas_efectivas |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname, contains("cualidades_valora_candidato_O"),pesos) |>
  mutate(tot_pesos = sum(pesos)) |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  pivot_longer(-tot_pesos, names_to = "respuesta",values_to = "value") %>%
  mutate(pct = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = pct,
                                          .fun = max))


p_cualidades_valora_candidato_graf <-
  bd_cualidades_valora_candidato |>
  graficar_lollipops(width_cats = 50) +
  scale_color_manual(values = colores_cualidades_valora_candidato) +
  scale_y_continuous(limits = c(0, 1.0),
                     labels = scales::percent) +
  labs(caption = p_cualidades_valora_candidato_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 12))

# Nesecidad economica chile
bd_necesita_chile_economia<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(necesita_chile_economia,pesos) |>
  count(necesita_chile_economia,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=necesita_chile_economia )

p_necesita_chile_economia_graf<-
  bd_necesita_chile_economia|>
  #graficar_barras(orden_respuestas = rev(orden_necesita_chile_economia))+
  graficar_barras(salto = 20,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.1)+
  scale_fill_manual(values = colores_necesita_chile_economia) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  labs(caption = p_necesita_chile_economia_tit) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 13),
        plot.caption = element_text(size = 12))


# Necesidad consesnso chile
bd_necesita_chile_consenso <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(necesita_chile_consenso,pesos) |>
  count(necesita_chile_consenso,wt = pesos) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=necesita_chile_consenso )

p_necesita_chile_consenso_graf <-
  bd_necesita_chile_consenso|>
  #graficar_barras(orden_respuestas = rev(orden_necesita_chile_consenso))+
  graficar_barras(salto = 20,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.1)+
  scale_fill_manual(values = colores_necesita_chile_consenso) +
  labs(caption = p_necesita_chile_consenso_tit)+
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  tema_morant()+
  theme(axis.text.x = element_text(size = 13),
        plot.caption = element_text(size = 12))




# bd_necesita_chile_consenso <-
#   bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(aprueba_gobierno_boric) |>
#   count(aprueba_gobierno_boric) |>
#   mutate(media = n /sum(n)) |>
#   rename(respuesta=necesita_chile_consenso )

