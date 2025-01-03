# Generar glosarios ---------------------------------------------------------------------------
path_bd_categorias <- "./data/bd_categorias_20241214_160h..xlsx"

bd_categorias_raw <-
  readxl::read_xlsx(path = path_bd_categorias, skip = 1)

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "nombre_prox_gb")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "prioridad_gob_maru")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razonopinion_per1_cruz_buena")
##

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razonopinion_per1_cruz_mala")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razonopinion_per1_andrea_buena")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razonopinion_per1_andrea_mala")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razonopinion_per2_bonilla_buena")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razonopinion_per2_bonilla_mala")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razonopinion_per2_humberto_buena")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razonopinion_per2_humberto_mala")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_maru_aprueba")

encuestar:::generarGlosario_preguntaAbierta(folder = "./glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw,
                                            variable = "razon_maru_desaprueba")

