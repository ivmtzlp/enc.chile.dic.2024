error_muestral_maximo(diccionario = diccionario |>
                        janitor::clean_names() |>
                        rename(respuestas =  respuesta,llaves = llave,tipo_pregunta =tipo_de_pregunta ),
                      diseno = calibrated_design,
                      quitar_patron = c("opinion_","calif_","razon_no_trabajo","intentos") )









