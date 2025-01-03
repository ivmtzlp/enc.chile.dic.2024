# Generar resultados --------------------------------------------------------------------------

# NOMBRE PROX GB 27
bd_categoria_nombre_prox_gb <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_nombre_prox_gb")

# Prioridad gob Maru
bd_categoria_prioridad_gob_maru <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_prioridad_gob_maru")

# Razon buena opinion cruz
pct_opinion_per1_cruz_buena <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "opinion_per1_cruz") |>
  filter(respuesta %in% c("Buena")) |>
  pull(media)

bd_categoria_razonopinion_per1_cruz_buena <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razonopinion_per1_cruz_buena")


# Razon mala opinion cruz
pct_opinion_per1_cruz_mala <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "opinion_per1_cruz") |>
  filter(respuesta %in% c("Mala")) |>
  pull(media)

bd_categoria_razonopinion_per1_cruz_mala <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razonopinion_per1_cruz_mala")



# Razon buena opinion andrea
pct_opinion_per1_andrea_buena <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "opinion_per1_andrea") |>
  filter(respuesta %in% c("Buena")) |>
  pull(media)

bd_categoria_razonopinion_per1_andrea_buena <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razonopinion_per1_andrea_buena")


# Razon mala opinion andrea
pct_opinion_per1_andrea_mala <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "opinion_per1_andrea") |>
  filter(respuesta %in% c("Mala")) |>
  pull(media)

bd_categoria_razonopinion_per1_andrea_mala <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razonopinion_per1_andrea_mala")



# Razon buena opinion bonilla
pct_opinion_per2_bonilla_buena <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "opinion_per2_bonilla") |>
  filter(respuesta %in% c("Buena")) |>
  pull(media)

bd_categoria_razonopinion_per2_bonilla_buena <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razonopinion_per2_bonilla_buena")


# Razon mala opinion bonilla
pct_opinion_per2_bonilla_mala <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "opinion_per2_bonilla") |>
  filter(respuesta %in% c("Mala")) |>
  pull(media)

bd_categoria_razonopinion_per2_bonilla_mala <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razonopinion_per2_bonilla_mala")


####

# Razon buena opinion humberto
pct_opinion_per2_humberto_buena <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "opinion_per2_humberto") |>
  filter(respuesta %in% c("Buena")) |>
  pull(media)

bd_categoria_razonopinion_per2_humberto_buena <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razonopinion_per2_humberto_buena")


# Razon mala opinion humberto
pct_opinion_per2_humberto_mala <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "opinion_per2_humberto") |>
  filter(respuesta %in% c("Mala")) |>
  pull(media)

bd_categoria_razonopinion_per2_humberto_mala <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razonopinion_per2_humberto_mala")




####

# Aprueba maru

pct_razon_Maru_aprueba <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "aprueba_gb") |>
  #filter(respuesta %in% c("Aprueba mucho","Aprueba poco")) |>
  filter(respuesta %in% c("Aprueba mucho")) |>
  pull(media) |>
  sum()

bd_categoria_razon_maru_aprueba <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razon_maru_aprueba")


# Desaprueba maru

pct_razon_Maru_desaprueba <-
  encuestar:::analizar_frecuencias(diseno = enc_chih$muestra$diseno,
                                   pregunta = "aprueba_gb") |>
  #filter(respuesta %in% c("Desaprueba mucho","Desaprueba poco")) |>
  filter(respuesta %in% c("Desaprueba mucho")) |>
  pull(media) |>
  sum()

bd_categoria_razon_maru_desaprueba <-
  encuestar::calcular_proporcionesCategorias(diseno = enc_chih$muestra$diseno,
                                             llave_categorias = "categoria_razon_maru_desaprueba")


