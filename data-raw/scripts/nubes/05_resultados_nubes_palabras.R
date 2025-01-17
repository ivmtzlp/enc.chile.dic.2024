source(file = "./data-raw/scripts/nubes/04_parametros_nubes_palabras.R")


### bachelet

# Negativa bachelet
glosario_razon_opinion_bachelet_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_bachelet_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_bachelet_negativa <-
  bd_categoria_razon_opinion_bachelet_negativa |>
  left_join(glosario_razon_opinion_bachelet_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_bachelet_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva bachelet
glosario_razon_opinion_bachelet_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_bachelet_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_bachelet_positiva <-
  bd_categoria_razon_opinion_bachelet_positiva |>
  left_join(glosario_razon_opinion_bachelet_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_bachelet_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()



### mathei

# Negativa mathei
glosario_razon_opinion_mathei_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_mathei_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_mathei_negativa <-
  bd_categoria_razon_opinion_mathei_negativa |>
  left_join(glosario_razon_opinion_mathei_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 35) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_mathei_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva mathei
glosario_razon_opinion_mathei_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_mathei_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_mathei_positiva <-
  bd_categoria_razon_opinion_mathei_positiva |>
  left_join(glosario_razon_opinion_mathei_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_mathei_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()




### ominami

# Negativa ominami
glosario_razon_opinion_ominami_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_ominami_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_ominami_negativa <-
  bd_categoria_razon_opinion_ominami_negativa |>
  left_join(glosario_razon_opinion_ominami_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 35) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_ominami_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva ominami
glosario_razon_opinion_ominami_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_ominami_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_ominami_positiva <-
  bd_categoria_razon_opinion_ominami_positiva |>
  left_join(glosario_razon_opinion_ominami_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_ominami_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()





### toha

# Negativa toha
glosario_razon_opinion_toha_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_toha_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_toha_negativa <-
  bd_categoria_razon_opinion_toha_negativa |>
  left_join(glosario_razon_opinion_toha_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_toha_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva toha
glosario_razon_opinion_toha_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_toha_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_toha_positiva <-
  bd_categoria_razon_opinion_toha_positiva |>
  left_join(glosario_razon_opinion_toha_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_toha_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()





### vodanovic

# Negativa vodanovic
glosario_razon_opinion_vodanovic_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_vodanovic_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_vodanovic_negativa <-
  bd_categoria_razon_opinion_vodanovic_negativa |>
  left_join(glosario_razon_opinion_vodanovic_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_vodanovic_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva vodanovic
glosario_razon_opinion_vodanovic_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_vodanovic_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_vodanovic_positiva <-
  bd_categoria_razon_opinion_vodanovic_positiva |>
  left_join(glosario_razon_opinion_vodanovic_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_vodanovic_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()





### winter

# Negativa winter
glosario_razon_opinion_winter_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_winter_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_winter_negativa <-
  bd_categoria_razon_opinion_winter_negativa |>
  left_join(glosario_razon_opinion_winter_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_winter_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva winter
glosario_razon_opinion_winter_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_winter_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_winter_positiva <-
  bd_categoria_razon_opinion_winter_positiva |>
  left_join(glosario_razon_opinion_winter_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_winter_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()


### parisi

# Negativa parisi
glosario_razon_opinion_parisi_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_parisi_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_parisi_negativa <-
  bd_categoria_razon_opinion_parisi_negativa |>
  left_join(glosario_razon_opinion_parisi_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_parisi_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva parisi
glosario_razon_opinion_parisi_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_parisi_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_parisi_positiva <-
  bd_categoria_razon_opinion_parisi_positiva |>
  left_join(glosario_razon_opinion_parisi_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 45) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_parisi_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()


### kast

# Negativa kast
glosario_razon_opinion_kast_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_kast_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_kast_negativa <-
  bd_categoria_razon_opinion_kast_negativa |>
  left_join(glosario_razon_opinion_kast_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_kast_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva kast
glosario_razon_opinion_kast_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_kast_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_kast_positiva <-
  bd_categoria_razon_opinion_kast_positiva |>
  left_join(glosario_razon_opinion_kast_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 45) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_kast_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()


### kaiser

# Negativa kaiser
glosario_razon_opinion_kaiser_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_kaiser_negativa.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_kaiser_negativa <-
  bd_categoria_razon_opinion_kaiser_negativa |>
  left_join(glosario_razon_opinion_kaiser_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 45) +
  labs(title = paste0("Negativa",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_kaiser_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva kaiser
glosario_razon_opinion_kaiser_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_opinion_kaiser_positiva.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_opinion_kaiser_positiva <-
  bd_categoria_razon_opinion_kaiser_positiva |>
  left_join(glosario_razon_opinion_kaiser_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 45) +
  labs(title = paste0("Positiva",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_opinion_kaiser_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()




#########################################################
# abureba boric
########################################################3
#
# Negativa kaiser
glosario_razon_aprueba_gobierno_boric_negativa <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_aprueba_gobierno_boric_desaprueba.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_aprueba_gobierno_boric_negativa <-
  bd_categoria_razon_aprueba_gobierno_boric_negativa |>
  left_join(glosario_razon_aprueba_gobierno_boric_negativa, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_mala, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Desaprueba",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_aprueba_gobierno_boric_negativa, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_mala,
                                  face = "bold"))+
  tema_transparente()


# Positiva kaiser
glosario_razon_aprueba_gobierno_boric_positiva <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_razon_aprueba_gobierno_boric_aprueba.xlsx") |>
  select(categoria, categoria_corregida)

wc_razon_aprueba_gobierno_boric_positiva <-
  bd_categoria_razon_aprueba_gobierno_boric_positiva |>
  left_join(glosario_razon_aprueba_gobierno_boric_positiva, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "top",
                                       top = 10,
                                       colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  labs(title = paste0("Aprueba",
                      "\n",
                      "Porcentaje con esa postura: ",
                      scales::percent(pct_aprueba_gobierno_boric_positiva, accuracy = 1.))) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_opinion_buena,
                                  face = "bold"))+
  tema_transparente()




##############################################################################################################
##############################################################################################################
#Describe palabra
##############################################################################################################
##############################################################################################################

# bachelet

glosario_describe_bachelet <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_describe_bachelet.xlsx") |>
  select(categoria, categoria_corregida)|>
  distinct(categoria,.keep_all = T)

wc_describe_bachelet <-
  bd_describe_bachelet |>
  left_join(glosario_describe_bachelet, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",cuantiles = 4) |>
  # encuestar::asignar_coloresCategorias(criterio = "top",
  #                                      top = 10,
  #                                      colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 35) +
  # labs(title = paste0("Describa en una sola palabra a\n",
  #                     "Michelle Bachelete")) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_general,
                                  face = "bold"))+
  tema_transparente()

# winter

glosario_describe_winter <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_describe_winter.xlsx") |>
  select(categoria, categoria_corregida)|>
  distinct(categoria,.keep_all = T)

wc_describe_winter <-
  bd_describe_winter |>
  left_join(glosario_describe_winter, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",cuantiles = 4) |>
  # encuestar::asignar_coloresCategorias(criterio = "top",
  #                                      top = 10,
  #                                      colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 55) +
  # labs(title = paste0("Describa en una sola palabra a\n",
  #                     "Gonzalo Winter")) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_general,
                                  face = "bold"))+
  tema_transparente()


# vodanovic

glosario_describe_vodanovic <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_describe_vodanovic.xlsx") |>
  select(categoria, categoria_corregida)|>
  distinct(categoria,.keep_all = T)

wc_describe_vodanovic <-
  bd_describe_vodanovic |>
  left_join(glosario_describe_vodanovic, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",cuantiles = 4) |>
  # encuestar::asignar_coloresCategorias(criterio = "top",
  #                                      top = 10,
  #                                      colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 40) +
  # labs(title = paste0("Describa en una sola palabra a\n",
  #                     "Tomás Vodanovic")) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_general,
                                  face = "bold"))+
  tema_transparente()

# ominami

glosario_describe_ominami <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_describe_ominami.xlsx") |>
  select(categoria, categoria_corregida)|>
  distinct(categoria,.keep_all = T)

wc_describe_ominami <-
  bd_describe_ominami |>
  left_join(glosario_describe_ominami, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",cuantiles = 4) |>
  # encuestar::asignar_coloresCategorias(criterio = "top",
  #                                      top = 10,
  #                                      colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  # labs(title = paste0("Describa en una sola palabra a\n",
  #                     "Marco Enríquez-Ominami")) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_general,
                                  face = "bold"))+
  tema_transparente()

# toha

glosario_describe_toha <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_describe_toha.xlsx") |>
  select(categoria, categoria_corregida)|>
  distinct(categoria,.keep_all = T)

wc_describe_toha <-
  bd_describe_toha |>
  left_join(glosario_describe_toha, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",cuantiles = 4) |>
  # encuestar::asignar_coloresCategorias(criterio = "top",
  #                                      top = 10,
  #                                      colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 35) +
  # labs(title = paste0("Describa en una sola palabra a\n",
  #                     "Carolina Tohá")) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_general,
                                  face = "bold"))+
  tema_transparente()

# mathei

glosario_describe_mathei <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_describe_mathei.xlsx") |>
  select(categoria, categoria_corregida)|>
  distinct(categoria,.keep_all = T)

wc_describe_mathei <-
  bd_describe_mathei |>
  left_join(glosario_describe_mathei, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",cuantiles = 4) |>
  # encuestar::asignar_coloresCategorias(criterio = "top",
  #                                      top = 10,
  #                                      colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 35) +
  # labs(title = paste0("Describa en una sola palabra a\n",
  #                     "Evelyn Mathei")) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_general,
                                  face = "bold"))+
  tema_transparente()

# kast

glosario_describe_kast <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_describe_kast.xlsx") |>
  select(categoria, categoria_corregida)|>
  distinct(categoria,.keep_all = T)

wc_describe_kast <-
  bd_describe_kast |>
  left_join(glosario_describe_kast, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",cuantiles = 4) |>
  # encuestar::asignar_coloresCategorias(criterio = "top",
  #                                      top = 10,
  #                                      colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 50) +
  # labs(title = paste0("Describa en una sola palabra a\n",
  #                     "José Antonio Kast")) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_general,
                                  face = "bold"))+
  tema_transparente()

# kaiser

glosario_describe_kaiser <-
  readxl::read_excel(path = "./data-raw/scripts/nubes/glosarios/glosario_describe_kaiser.xlsx") |>
  select(categoria, categoria_corregida)|>
  distinct(categoria,.keep_all = T)

wc_describe_kaiser <-
  bd_describe_kaiser |>
  left_join(glosario_describe_kaiser, by = "categoria") |>
  encuestar::asignar_coloresCategorias(criterio = "cuantiles",cuantiles = 4) |>
  # encuestar::asignar_coloresCategorias(criterio = "top",
  #                                      top = 10,
  #                                      colores = c(color_opinion_buena, color_nsnc)) |>
  encuestar:::graficar_nube_palabras(max_size = 55) +
  # labs(title = paste0("Describa en una sola palabra a\n",
  #                     "Johannes Kaiser")) +
  theme(plot.title = element_text(size = 26,
                                  colour = color_general,
                                  face = "bold"))+
  tema_transparente()

