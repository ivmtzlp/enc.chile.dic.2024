
# bd_respuestas_efectivas |>
#   as_tibble() |>
#   select(participacion_primarias)|>
#   naniar::vis_miss()

source('./data-raw/scripts/parametros/parametros_bloque_participacion_politica.R')

#######################################333

# Interes Politica
bd_interes_politica<-
bd_respuestas_efectivas |>
  as_tibble() |>
  select(interes_politica) |>
  count(interes_politica) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=interes_politica )


p_interes_politica_graf<-
bd_interes_politica|>
  graficar_barras(orden_respuestas = rev(orden_interes_politica),salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02)+
  scale_fill_manual(values=colores_interes_politica)+
  labs(caption = p_interes_politica_tit) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



# Interes eleccion municipal
bd_interes_eleccion_mun_24<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(interes_eleccion_mun_24) |>
  count(interes_eleccion_mun_24) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=interes_eleccion_mun_24 )


p_interes_eleccion_mun_24_graf <-
  bd_interes_eleccion_mun_24|>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(orden_interes_politica))+

  labs(caption = p_interes_eleccion_mun_24_tit)+
  scale_fill_manual(values = colores_interes_eleccion_mun_24) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



# Perticipacion presidencia 21
bd_participacion_pr_21<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_pr_21) |>
  count(participacion_pr_21) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_pr_21 ) |>
  filter(respuesta == 'Sí')


p_participacion_pr_21_graf <-
bd_participacion_pr_21 |>
  graficar_gauge(color_principal = color_general,escala = c(0,1),size_text_pct = 12)+
  labs(title = p_participacion_pr_21_tit,
       caption =  "Entrevistados que contestaron que Sí")+
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(size = 12))


# Perticipacion municipal 24
bd_participacion_mun_24<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_mun_24) |>
  count(participacion_mun_24) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_mun_24 ) |>
  filter(respuesta == 'Sí')


p_participacion_mun_24_graf <-
  bd_participacion_mun_24 |>
  graficar_gauge(color_principal =color_general,escala = c(0,1),size_text_pct = 12)+
  labs(title = p_participacion_mun_24_tit,
       caption =  "Entrevistados que contestaron que Sí")+
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(size = 12))



#Voto proximas elecciones
bd_voto_proximas_elecciones<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_proximas_elecciones) |>
  count(voto_proximas_elecciones) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_proximas_elecciones)


p_voto_proximas_elecciones_graf<-
  bd_voto_proximas_elecciones|>
  graficar_barras(salto = 35,
                  porcentajes_fuera = TRUE,
                  text_size = 6,
                  desplazar_porcentajes = 0.02)+
  #graficar_barras(orden_respuestas = rev(orden_voto_proximas_elecciones))+
  scale_fill_manual(values = colores_voto_proximas_elecciones) +
  labs(caption = p_voto_proximas_elecciones_tit)+
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant() +
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


# Perticipacion primarias 25
bd_participacion_primarias<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(participacion_primarias) |>
  count(participacion_primarias) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=participacion_primarias ) |>
  filter(respuesta == 'Sí')


p_participacion_primarias_graf <-
  bd_participacion_primarias |>
  graficar_gauge(color_principal = color_general,escala = c(0,1),size_text_pct = 12)+
  labs(title = p_participacion_primarias_tit,
       caption =  "Entrevistados que contestaron que Sí")+
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(size = 12))


# Voto Pr 25
bd_voto_pr<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto_pr) |>
  filter(!is.na(voto_pr)) |>
  count(voto_pr) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto_pr)


p_voto_pr_graf<-
  bd_voto_pr|>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.05)+
  #graficar_barras(orden_respuestas = rev(orden_voto_pr))+
  scale_fill_manual(values = colores_voto_pr ) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  labs(caption = p_voto_pr_tit)+
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


# Voto segundo Pr 25
bd_voto2_pr<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(voto2_pr) |>
  filter(!is.na(voto2_pr)) |>
  count(voto2_pr) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=voto2_pr)


p_voto2_pr_graf<-
  bd_voto2_pr|>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.05)+
  #graficar_barras(orden_respuestas = rev(orden_voto2_pr))+
  labs(caption = p_voto2_pr_tit)+
  scale_fill_manual(values = colores_voto2_pr ) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))



# candidato nunca voto pr 25
bd_candidato_nunca_voto<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(candidato_nunca_voto) |>
  filter(!is.na(candidato_nunca_voto)) |>
  count(candidato_nunca_voto) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=candidato_nunca_voto)


p_candidato_nunca_voto_graf<-
  bd_candidato_nunca_voto|>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.02)+
  #graficar_barras(orden_respuestas = rev(orden_candidato_nunca_voto))+
  labs(caption = p_candidato_nunca_voto_tit)+
  scale_fill_manual(values = colores_candidato_nunca_voto ) +
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


# postura ideologica
bd_definicion_postura_ideologica<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(definicion_postura_ideologica) |>
  #filter(!is.na(definicion_postura_ideologica)) |>
  count(definicion_postura_ideologica) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=definicion_postura_ideologica)



p_definicion_postura_ideologica_graf<-
  bd_definicion_postura_ideologica|>
  #graficar_barras()+
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.02,
                  orden_respuestas = rev(orden_definicion_postura_ideologica))+
  scale_y_continuous(limits = c(0, 0.5),
                     labels = scales::percent) +
  scale_fill_manual(values = colores_definicion_postura_ideologica) +
  labs(caption = p_definicion_postura_ideologica_tit)+
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))




# Identificacion partidista
bd_identificacion_partido<-
  bd_respuestas_efectivas |>
  as_tibble() |>
  select(identificacion_partido) |>
  #filter(!is.na(identificacion_partido)) |>
  count(identificacion_partido) |>
  mutate(media = n /sum(n)) |>
  rename(respuesta=identificacion_partido)


p_identificacion_partido_graf<-
  bd_identificacion_partido|>
  graficar_barras(salto = 35,
                  text_size = 6,
                  porcentajes_fuera = TRUE,
                  desplazar_porcentajes = 0.03)+
  #graficar_barras(orden_respuestas = rev(orden_identificacion_partido))+
  scale_y_continuous(limits = c(0, 0.75),
                     labels = scales::percent) +
  scale_fill_manual(values = colores_identificacion_partido) +
  labs(caption = p_identificacion_partido_tit)+
  tema_morant()+
  theme(axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 12))


















