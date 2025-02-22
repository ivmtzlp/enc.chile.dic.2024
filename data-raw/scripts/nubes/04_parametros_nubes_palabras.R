# Generar resultados --------------------------------------------------------------------------


### bachelet

pct_opinion_bachelet <-
  bd_respuestas_efectivas |>
  count(opinion_bachelet,wt = pesos) |>
  rename(respuesta = opinion_bachelet) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa bachelet
bd_categoria_razon_opinion_bachelet_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                             llave_categorias = "categoria_razon_opinion_bachelet_negativa")

pct_opinion_bachelet_negativa <-
pct_opinion_bachelet|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)

# Positiva bachelet
bd_categoria_razon_opinion_bachelet_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_bachelet_positiva")

pct_opinion_bachelet_positiva <-
  pct_opinion_bachelet|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)


### mathei

pct_opinion_mathei <-
  bd_respuestas_efectivas |>
  count(opinion_mathei,wt = pesos) |>
  rename(respuesta = opinion_mathei) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa mathei
bd_categoria_razon_opinion_mathei_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_mathei_negativa")

pct_opinion_mathei_negativa <-
  pct_opinion_mathei|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)

# Positiva mathei
bd_categoria_razon_opinion_mathei_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_mathei_positiva")

pct_opinion_mathei_positiva <-
  pct_opinion_mathei|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)


### ominami

pct_opinion_ominami <-
  bd_respuestas_efectivas |>
  count(opinion_ominami,wt = pesos) |>
  rename(respuesta = opinion_ominami) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa ominami
bd_categoria_razon_opinion_ominami_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_ominami_negativa")

pct_opinion_ominami_negativa <-
  pct_opinion_ominami|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)

# Positiva ominami
bd_categoria_razon_opinion_ominami_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_ominami_positiva")

pct_opinion_ominami_positiva <-
  pct_opinion_ominami|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)


### toha

pct_opinion_toha <-
  bd_respuestas_efectivas |>
  count(opinion_toha,wt = pesos) |>
  rename(respuesta = opinion_toha) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))

# Negativa toha
bd_categoria_razon_opinion_toha_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_toha_negativa")

pct_opinion_toha_negativa <-
  pct_opinion_toha|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)


# Positiva toha
bd_categoria_razon_opinion_toha_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_toha_positiva")


pct_opinion_toha_positiva <-
  pct_opinion_toha|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)


### vodanovic

pct_opinion_vodanovic <-
  bd_respuestas_efectivas |>
  count(opinion_vodanovic,wt = pesos) |>
  rename(respuesta = opinion_vodanovic) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa vodanovic
bd_categoria_razon_opinion_vodanovic_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_vodanovic_negativa")


pct_opinion_vodanovic_negativa <-
  pct_opinion_vodanovic|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)


# Positiva vodanovic
bd_categoria_razon_opinion_vodanovic_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_vodanovic_positiva")


pct_opinion_vodanovic_positiva <-
  pct_opinion_vodanovic|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)


### winter

pct_opinion_winter <-
  bd_respuestas_efectivas |>
  count(opinion_winter,wt = pesos) |>
  rename(respuesta = opinion_winter) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa winter
bd_categoria_razon_opinion_winter_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_winter_negativa")


pct_opinion_winter_negativa <-
  pct_opinion_winter|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)


# Positiva winter
bd_categoria_razon_opinion_winter_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_winter_positiva")


pct_opinion_winter_positiva <-
  pct_opinion_winter|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)



### parisi

pct_opinion_parisi <-
  bd_respuestas_efectivas |>
  count(opinion_parisi,wt = pesos) |>
  rename(respuesta = opinion_parisi) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa parisi
bd_categoria_razon_opinion_parisi_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_parisi_negativa")


pct_opinion_parisi_negativa <-
  pct_opinion_parisi|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)


# Positiva parisi
bd_categoria_razon_opinion_parisi_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_parisi_positiva")


pct_opinion_parisi_positiva <-
  pct_opinion_parisi|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)



### kast

pct_opinion_kast <-
  bd_respuestas_efectivas |>
  count(opinion_kast,wt = pesos) |>
  rename(respuesta = opinion_kast) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa kast
bd_categoria_razon_opinion_kast_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_kast_negativa")


pct_opinion_kast_negativa <-
  pct_opinion_kast|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)


# Positiva kast
bd_categoria_razon_opinion_kast_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_kast_positiva")


pct_opinion_kast_positiva <-
  pct_opinion_kast|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)


### kaiser

pct_opinion_kaiser <-
  bd_respuestas_efectivas |>
  count(opinion_kaiser,wt = pesos) |>
  rename(respuesta = opinion_kaiser) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa kaiser
bd_categoria_razon_opinion_kaiser_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_kaiser_negativa")


pct_opinion_kaiser_negativa <-
  pct_opinion_kaiser|>
  filter(respuesta %in% c("Negativa")) |>
  pull(media)


# Positiva kaiser
bd_categoria_razon_opinion_kaiser_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_opinion_kaiser_positiva")


pct_opinion_kaiser_positiva <-
  pct_opinion_kaiser|>
  filter(respuesta %in% c("Positiva")) |>
  pull(media)


######################################################################################
# Aprobacion gobierno borich
###

pct_aprueba_gobierno_boric <-
  bd_respuestas_efectivas |>
  count(aprueba_gobierno_boric,wt = pesos) |>
  rename(respuesta = aprueba_gobierno_boric) |>
  filter(!is.na(respuesta)) |>
  mutate(media = n/sum(n))


# Negativa
bd_categoria_razon_aprueba_gobierno_boric_negativa <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_aprueba_gobierno_boric_desaprueba")


pct_aprueba_gobierno_boric_negativa <-
  pct_aprueba_gobierno_boric|>
  filter(respuesta %in% c("Desaprueba mucho","Desaprueba poco")) |>
  summarise(media = sum(media)) |>
  pull(media)


# Positiva
bd_categoria_razon_aprueba_gobierno_boric_positiva <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_razon_aprueba_gobierno_boric_aprueba")


pct_aprueba_gobierno_boric_positiva <-
  pct_aprueba_gobierno_boric|>
  filter(respuesta %in% c("Aprueba mucho","Aprueba poco")) |>
  summarise(media = sum(media)) |>
  pull(media)

##################################################################################################33
#describe bachelet
bd_describe_bachelet <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_describe_bachelet")


#describe winter
bd_describe_winter <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_describe_winter")

#describe vodanovic
bd_describe_vodanovic <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_describe_vodanovic")
#describe ominami
bd_describe_ominami <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_describe_ominami")
#describe toha
bd_describe_toha <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_describe_toha")
#describe mathei
bd_describe_mathei <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_describe_mathei")
#describe kast
bd_describe_kast <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_describe_kast")
#describe kaiser
bd_describe_kaiser <-
  calcular_proporcionesCategorias(bd = bd_respuestas_efectivas,
                                  llave_categorias = "categoria_describe_kaiser")
















#
# # Razon buena opinion andrea
# pct_opinion_per1_andrea_buena <-
#   :analizar_frecuencias(diseno = bd_respuestas_efectivas,
#                                    pregunta = "opinion_per1_andrea") |>
#   filter(respuesta %in% c("Buena")) |>
#   pull(media)
#
# bd_categoria_razonopinion_per1_andrea_buena <-
#   calcular_proporcionesCategorias(diseno = bd_respuestas_efectivas,
#                                              llave_categorias = "categoria_razonopinion_per1_andrea_buena")
#
#
# # Razon mala opinion andrea
# pct_opinion_per1_andrea_mala <-
#   :analizar_frecuencias(diseno = bd_respuestas_efectivas,
#                                    pregunta = "opinion_per1_andrea") |>
#   filter(respuesta %in% c("Mala")) |>
#   pull(media)
#
# bd_categoria_razonopinion_per1_andrea_mala <-
#   calcular_proporcionesCategorias(diseno = bd_respuestas_efectivas,
#                                              llave_categorias = "categoria_razonopinion_per1_andrea_mala")
#
#
#
# # Razon buena opinion bonilla
# pct_opinion_per2_bonilla_buena <-
#   :analizar_frecuencias(diseno = bd_respuestas_efectivas,
#                                    pregunta = "opinion_per2_bonilla") |>
#   filter(respuesta %in% c("Buena")) |>
#   pull(media)
#
# bd_categoria_razonopinion_per2_bonilla_buena <-
#   calcular_proporcionesCategorias(diseno = bd_respuestas_efectivas,
#                                              llave_categorias = "categoria_razonopinion_per2_bonilla_buena")
#
#
# # Razon mala opinion bonilla
# pct_opinion_per2_bonilla_mala <-
#   :analizar_frecuencias(diseno = bd_respuestas_efectivas,
#                                    pregunta = "opinion_per2_bonilla") |>
#   filter(respuesta %in% c("Mala")) |>
#   pull(media)
#
# bd_categoria_razonopinion_per2_bonilla_mala <-
#   calcular_proporcionesCategorias(diseno = bd_respuestas_efectivas,
#                                              llave_categorias = "categoria_razonopinion_per2_bonilla_mala")
#
#
# ####
#
# # Razon buena opinion humberto
# pct_opinion_per2_humberto_buena <-
#   :analizar_frecuencias(diseno = bd_respuestas_efectivas,
#                                    pregunta = "opinion_per2_humberto") |>
#   filter(respuesta %in% c("Buena")) |>
#   pull(media)
#
# bd_categoria_razonopinion_per2_humberto_buena <-
#   calcular_proporcionesCategorias(diseno = bd_respuestas_efectivas,
#                                              llave_categorias = "categoria_razonopinion_per2_humberto_buena")
#
#
# # Razon mala opinion humberto
# pct_opinion_per2_humberto_mala <-
#   :analizar_frecuencias(diseno = bd_respuestas_efectivas,
#                                    pregunta = "opinion_per2_humberto") |>
#   filter(respuesta %in% c("Mala")) |>
#   pull(media)
#
# bd_categoria_razonopinion_per2_humberto_mala <-
#   calcular_proporcionesCategorias(diseno = bd_respuestas_efectivas,
#                                              llave_categorias = "categoria_razonopinion_per2_humberto_mala")
#
#
#
#
# ####
#
# # Aprueba maru
#
# pct_razon_Maru_aprueba <-
#   :analizar_frecuencias(diseno = bd_respuestas_efectivas,
#                                    pregunta = "aprueba_gb") |>
#   #filter(respuesta %in% c("Aprueba mucho","Aprueba poco")) |>
#   filter(respuesta %in% c("Aprueba mucho")) |>
#   pull(media) |>
#   sum()
#
# bd_categoria_razon_maru_aprueba <-
#   calcular_proporcionesCategorias(diseno = bd_respuestas_efectivas,
#                                              llave_categorias = "categoria_razon_maru_aprueba")
#
#
# # Desaprueba maru
#
# pct_razon_Maru_desaprueba <-
#   :analizar_frecuencias(diseno = bd_respuestas_efectivas,
#                                    pregunta = "aprueba_gb") |>
#   #filter(respuesta %in% c("Desaprueba mucho","Desaprueba poco")) |>
#   filter(respuesta %in% c("Desaprueba mucho")) |>
#   pull(media) |>
#   sum()
#
# bd_categoria_razon_maru_desaprueba <-
#   calcular_proporcionesCategorias(diseno = bd_respuestas_efectivas,
#                                              llave_categorias = "categoria_razon_maru_desaprueba")
#
#
