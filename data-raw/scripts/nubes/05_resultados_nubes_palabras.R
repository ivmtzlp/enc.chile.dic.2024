source(file = "./data-raw/scripts/nubes/04_parametros_nubes_palabras.R")

# Prioridad gob Maru
glosario_categoria_prioridad_gob_maru <-
  readxl::read_excel(path = "./glosarios/glosario_prioridad_gob_maru.xlsx") |>
  select(categoria, categoria_corregida)

wc_prioridad_gob_maru <-
  bd_categoria_prioridad_gob_maru |>
  left_join(glosario_categoria_prioridad_gob_maru, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",
                                       cuantiles = 4,
                                       colores = colores_cuantiles) |>
  encuestar:::graficar_nube_palabras(max_size = 90)+
  labs(caption = '¿Cuál cree que ha sido la prioridad del gobierno de Maru Campos?, \n¿en qué ha trabajado más o hay una mejoría mayor?')+
  theme(plot.caption = element_text(family = 'Poppins',size = 14))

# NOMBRE PROX GB 27
glosario_categoria_nombre_prox_gb <-
  readxl::read_excel(path = "./glosarios/glosario_nombre_prox_gb.xlsx") |>
  select(categoria, categoria_corregida)

bd_nombre_prox_gb <-
  bd_categoria_nombre_prox_gb |>
  left_join(glosario_categoria_nombre_prox_gb, by = "categoria") |>
  transmute(respuesta = categoria_corregida,
            media = pct) |>
  mutate(respuesta = forcats::fct_lump_min(f = respuesta, min = 0.02, w = media, other_level = "Otros"),
         respuesta = dplyr::if_else(condition = respuesta == "Otro", true = "Otros", false = respuesta)) %>%
  group_by(respuesta) |>
  summarise(media = sum(media))

colores_nombre_prox_gb <-
  bd_nombre_prox_gb |>
  distinct(respuesta) |>
  asignar_colores()

wc_nombre_prox_gb <-
  bd_nombre_prox_gb |>
  encuestar:::graficar_barras(text_size = 14) +
  tema_morant() +
  scale_fill_manual(values = c("Andrea Chávez" = color_morena,
                               "Cruz Pérez Cuéllar" = color_morena,
                               "Marco Bonilla" = color_general,
                               "Maru Campos" = color_general,
                               "No sabe/ Ninguno" = color_nsnc,
                               "Otros" = color_otro))+
  labs(caption = '¿Qué persona cree que podría ser el\n próximo Gobernador de Chihuahua? ')

# Razon buena opinion
glosario_categoria_razonopinion_per1_cruz_buena <-
  readxl::read_excel(path = "./glosarios/glosario_razonopinion_per1_cruz_buena.xlsx") |>
  select(categoria, categoria_corregida)

wc_razonopinion_per1_cruz_buena <-
  bd_categoria_razonopinion_per1_cruz_buena |>
  left_join(glosario_categoria_razonopinion_per1_cruz_buena, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  labs(title = paste0("Buena",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_per1_cruz_buena, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))

# Razon mala opinion
glosario_categoria_razonopinion_per1_cruz_mala <-
  readxl::read_excel(path = "./glosarios/glosario_razonopinion_per1_cruz_mala.xlsx") |>
  select(categoria, categoria_corregida)

wc_razonopinion_per1_cruz_mala <-
  bd_categoria_razonopinion_per1_cruz_mala |>
  left_join(glosario_categoria_razonopinion_per1_cruz_mala, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  labs(title = paste0("Mala",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_per1_cruz_mala, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))

# Razon buena opinion
glosario_categoria_razonopinion_per1_andrea_buena <-
  readxl::read_excel(path = "./glosarios/glosario_razonopinion_per1_andrea_buena.xlsx") |>
  select(categoria, categoria_corregida)

wc_razonopinion_per1_andrea_buena <-
  bd_categoria_razonopinion_per1_andrea_buena |>
  left_join(glosario_categoria_razonopinion_per1_andrea_buena, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 41) +
  labs(title = paste0("Buena",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_per1_andrea_buena, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))

# Razon mala opinion
glosario_categoria_razonopinion_per1_andrea_mala <-
  readxl::read_excel(path = "./glosarios/glosario_razonopinion_per1_andrea_mala.xlsx") |>
  select(categoria, categoria_corregida)

wc_razonopinion_per1_andrea_mala <-
  bd_categoria_razonopinion_per1_andrea_mala |>
  left_join(glosario_categoria_razonopinion_per1_andrea_mala, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Mala",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_per1_andrea_mala, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))

# Razon buena opinion
glosario_categoria_razonopinion_per2_bonilla_buena <-
  readxl::read_excel(path = "./glosarios/glosario_razonopinion_per2_bonilla_buena.xlsx") |>
  select(categoria, categoria_corregida) |>
  distinct(categoria, categoria_corregida)

wc_razonopinion_per2_bonilla_buena <-
  bd_categoria_razonopinion_per2_bonilla_buena |>
  left_join(glosario_categoria_razonopinion_per2_bonilla_buena, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 45) +
  labs(title = paste0("Buena",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_per2_bonilla_buena, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))

# Razon mala opinion
glosario_categoria_razonopinion_per2_bonilla_mala <-
  readxl::read_excel(path = "./glosarios/glosario_razonopinion_per2_bonilla_mala.xlsx") |>
  select(categoria, categoria_corregida)

wc_razonopinion_per2_bonilla_mala <-
  bd_categoria_razonopinion_per2_bonilla_mala |>
  left_join(glosario_categoria_razonopinion_per2_bonilla_mala, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  labs(title = paste0("Mala",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_per2_bonilla_mala, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))

# Razon buena opinion
glosario_categoria_razonopinion_per2_humberto_buena <-
  readxl::read_excel(path = "./glosarios/glosario_razonopinion_per2_humberto_buena.xlsx") |>
  select(categoria, categoria_corregida)

wc_razonopinion_per2_humberto_buena <-
  bd_categoria_razonopinion_per2_humberto_buena |>
  left_join(glosario_categoria_razonopinion_per2_humberto_buena, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 45) +
  labs(title = paste0("Buena",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_per2_humberto_buena, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))

# Razon mala opinion
glosario_categoria_razonopinion_per2_humberto_mala <-
  readxl::read_excel(path = "./glosarios/glosario_razonopinion_per2_humberto_mala.xlsx") |>
  select(categoria, categoria_corregida)

wc_razonopinion_per2_humberto_mala <-
  bd_categoria_razonopinion_per2_humberto_mala |>
  left_join(glosario_categoria_razonopinion_per2_humberto_mala, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  labs(title = paste0("Mala",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_per2_humberto_mala, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))

# Aprueba Maru
glosario_categoria_razon_maru_aprueba <-
  readxl::read_excel(path = "./glosarios/glosario_razon_maru_aprueba.xlsx") |>
  select(categoria, categoria_corregida)

wc_razono_maru_aprueba <-
  bd_categoria_razon_maru_aprueba |>
  left_join(glosario_categoria_razon_maru_aprueba, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 38) +
  labs(title = paste0("Aprueba mucho",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_razon_Maru_aprueba, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))

# desaprueba Maru
glosario_categoria_razon_maru_desaprueba <-
  readxl::read_excel(path = "./glosarios/glosario_razon_maru_desaprueba.xlsx") |>
  select(categoria, categoria_corregida)

wc_razono_maru_desaprueba <-
  bd_categoria_razon_maru_desaprueba |>
  left_join(glosario_categoria_razon_maru_desaprueba, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 47) +
  labs(title = paste0("Desaprueba mucho",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_razon_Maru_desaprueba, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))
