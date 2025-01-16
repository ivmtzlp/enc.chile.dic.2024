encuesta_demo$muestra$diseno$variables <-
  encuesta_demo$muestra$diseno$variables |>
  mutate(stimuli_1 = sample(1:7,size = 1200,replace = T),
         stimuli_2 = sample(1:7,size = 1200,replace = T),
         stimuli_3 = sample(1:7,size = 1200,replace = T),
         stimuli_4 = sample(1:7,size = 1200,replace = T),
         stimuli_5 = sample(1:7,size = 1200,replace = T),
         stimuli_6 = sample(1:7,size = 1200,replace = T),
         stimuli_7 = sample(1:7,size = 1200,replace = T)
  )

encuesta_demo$muestra$diseno$variables|>
  names()



encuesta_demo$muestra$diseno$variables|>
  encuestar:::analizar_blackbox_1d(vars = c('stimuli_1','stimuli_2','stimuli_3',
                                            'stimuli_4','stimuli_5','stimuli_6','stimuli_7'),
                                   stimuli = 'voto_pr_24')|>
  graficar_blackbox_1d()



escala_izq_der_vec<- diccionario |>
  select(llave) |>
  filter(grepl("escala_",llave )) |>
  pull()





bd_respuestas_efectivas |>
  select(all_of(escala_izq_der_vec),voto_pr) |>
  mutate(across(.col =  all_of(escala_izq_der_vec),.fns = ~ifelse(. == "Ns/Nc",NA,.)
                  )) |>
  mutate(across(.col =  all_of(escala_izq_der_vec),.fns = ~as.numeric(.)
  )) |>
  filter(!is.na(voto_pr)) |>
  encuestar:::analizar_blackbox_1d(vars = escala_izq_der_vec,
                                   stimuli = 'voto_pr')|>
  encuestar:::graficar_blackbox_1d()




sample(1:7,size = 1200,replace = T)|>
  hist()
