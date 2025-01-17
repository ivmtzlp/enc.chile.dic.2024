# Generar glosarios ---------------------------------------------------------------------------
path_bd_categorias <- "./data-raw/bd_categorias.xlsx"

bd_categorias_raw <-
  readxl::read_xlsx(path = path_bd_categorias, skip = 1) |>
  rename(razon_opinion_bachelet_positiva = razon_opinon_bachelet_positiva,razon_opinion_bachelet_negativa=razon_opinon_bachelet_negativa)


##

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_bachelet_positiva")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_bachelet_negativa")


encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_mathei_negativa")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_mathei_positiva")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_ominami_negativa")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_ominami_positiva")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_toha_negativa")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_toha_positiva")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_vodanovic_negativa")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_vodanovic_positiva")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_winter_negativa")

encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_opinion_winter_positiva")


glos_faltantes_v1<- c("describe_bachelet","describe_ominami","describe_vodanovic","describe_winter","razon_opinion_kaiser_negativa",
  "razon_opinion_kaiser_positiva","razon_opinion_kast_negativa","razon_opinion_kast_positiva","razon_opinion_parisi_negativa",
  "razon_opinion_parisi_positiva")

for(element in glos_faltantes_v1){
  encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                              prefijo = "glosario_",
                                              bd_categorias_raw = bd_categorias_raw,
                                              variable = element)}





glos_faltantes_v2<- c("describe_kaiser","describe_kast","describe_mathei","describe_toha","razon_aprueba_gobierno_boric_aprueba","razon_aprueba_gobierno_boric_desaprueba")

for(element in glos_faltantes_v2){
  encuestar:::generarGlosario_preguntaAbierta(folder = "./data-raw/scripts/nubes/glosarios/",
                                              prefijo = "glosario_",
                                              bd_categorias_raw = bd_categorias_raw,
                                              variable = element)}


