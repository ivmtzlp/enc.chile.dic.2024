


diccionario |> View()

options(survey.lonely.psu="remove")

encuestar:::analizar_cruce(diseno = calibrated_design,
                           variable_principal = "utiliza_whatsapp",
                           variable_secundaria = "sexo",vartype = "se"
                           ) |>
    mutate(variable = "utiliza_whatsapp",
           cruce = "sexo") |>
  rename( "respuesta" =  "utiliza_whatsapp",  "valor_cruce" = "sexo", "se"="_se") |>
  # Por edad
  bind_rows(
    encuestar:::analizar_cruce(diseno = calibrated_design,
                               variable_principal = "utiliza_whatsapp",
                               variable_secundaria = "grupo_edad",vartype = "se"
    )|>
      mutate(variable = "utiliza_whatsapp",
             cruce = "grupo_edad") |>
      rename( "respuesta" =  "utiliza_whatsapp",  "valor_cruce" = "grupo_edad", "se"="_se")
  ) |>
  # Por nivel socieconomico
  bind_rows(
    encuestar:::analizar_cruce(diseno = calibrated_design,
                               variable_principal = "utiliza_whatsapp",
                               variable_secundaria = "niv_soc",vartype = "se"
    )|>
      mutate(variable = "utiliza_whatsapp",
             cruce = "niv_soc") |>
      rename( "respuesta" =  "utiliza_whatsapp",  "valor_cruce" = "niv_soc", "se"="_se")
  )


#####################################################################


llaves_vars<- diccionario |>
  filter(!bloque %in% c("Registro de ubicación","Filtros")) |>
  filter(`tipo de pregunta` %in% c("multiple")) |>
  filter(!grepl("problema_chile_O|cualidades_valora_candidato_O",llave)) |>  pull(llave)




nombres_base<- calibrated_design$variables |>
  names()



nombres_base[!nombres_base %in% llaves_vars]

base_cruces_gral <-
llaves_vars |>
  purrr::map_df(.f = ~{
    encuestar:::analizar_cruce(diseno = calibrated_design,
                               variable_principal = .,
                               variable_secundaria = "sexo",vartype = "se"
    ) |>
      mutate(variable = .,
             cruce = "sexo") |>
      rename( "respuesta" =  .,  "valor_cruce" = "sexo", "se"="_se") |>
      # Por edad
      bind_rows(
        encuestar:::analizar_cruce(diseno = calibrated_design,
                                   variable_principal = .,
                                   variable_secundaria = "grupo_edad",vartype = "se"
        )|>
          mutate(variable = .,
                 cruce = "grupo_edad") |>
          rename( "respuesta" =  .,  "valor_cruce" = "grupo_edad", "se"="_se")
      ) |>
      # Por nivel socieconomico
      bind_rows(
        encuestar:::analizar_cruce(diseno = calibrated_design,
                                   variable_principal = .,
                                   variable_secundaria = "niv_soc",vartype = "se"
        )|>
          mutate(variable = .,
                 cruce = "niv_soc") |>
          rename( "respuesta" =  .,  "valor_cruce" = "niv_soc", "se"="_se")
      )
  })



######
# problemas
######

base_cruces_gral_problemas<-
bd_respuestas_efectivas |>
  filter(sexo != "Ns/Nc (No leer)") |>
  #mutate(sexo = ifelse(sexo == "Ns/Nc (No leer)","Ns/Nc",sexo )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,sexo ,contains("problema_chile_O"),pesos) |>
  #group_by(sexo) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,sexo,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  distinct(rowname,sexo, pesos, tot_pesos, value,seleccion) |>
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(sexo) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,sexo), names_to = "respuesta",values_to = "value") %>%
  #group_by(sexo) |>
  mutate(coef = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = coef,
                                          .fun = max))|>
  mutate(variable = sexo,
         cruce = "sexo",
         se = 0) |>
  rename(   "valor_cruce" = "sexo") |>
  select(-c(tot_pesos,value)) |>
  # calculo por edad
  bind_rows(

    bd_respuestas_efectivas |>
      filter(grupo_edad != "Ns/Nc (No leer)") |>
      #mutate(grupo_edad = ifelse(grupo_edad == "Ns/Nc (No leer)","Ns/Nc",grupo_edad )) |>
      tibble::rownames_to_column() %>%
      as_tibble() |>
      select(rowname,grupo_edad ,contains("problema_chile_O"),pesos) |>
      #group_by(grupo_edad) |>
      mutate(tot_pesos = sum(pesos)) |>
      ungroup() |>
      tidyr::pivot_longer(cols = !c(rowname,pesos,grupo_edad,tot_pesos)) |>
      filter(!is.na(value)) %>%
      mutate(seleccion = 1) %>%
      select(-name) %>%
      distinct(rowname,grupo_edad, pesos, tot_pesos, value,seleccion) |>
      tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
      select(-rowname) %>%
      group_by(grupo_edad) |>
      summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
                tot_pesos = unique(tot_pesos) ) %>%
      tidyr::pivot_longer(-c(tot_pesos,grupo_edad), names_to = "respuesta",values_to = "value") %>%
      #group_by(grupo_edad) |>
      mutate(coef = value/tot_pesos,
             respuesta = forcats::fct_reorder(.f = respuesta,
                                              .x = coef,
                                              .fun = max))|>
      mutate(variable = grupo_edad,
             cruce = "grupo_edad",
             se = 0) |>
      rename(   "valor_cruce" = "grupo_edad") |>
      select(-c(tot_pesos,value))

  ) |>
  # calculo por grup economico
  bind_rows(

    bd_respuestas_efectivas |>
      filter(niv_soc != "Ns/Nc (No leer)") |>
      #mutate(niv_soc = ifelse(niv_soc == "Ns/Nc (No leer)","Ns/Nc",niv_soc )) |>
      tibble::rownames_to_column() %>%
      as_tibble() |>
      select(rowname,niv_soc ,contains("problema_chile_O"),pesos) |>
      #group_by(niv_soc) |>
      mutate(tot_pesos = sum(pesos)) |>
      ungroup() |>
      tidyr::pivot_longer(cols = !c(rowname,pesos,niv_soc,tot_pesos)) |>
      filter(!is.na(value)) %>%
      mutate(seleccion = 1) %>%
      select(-name) %>%
      distinct(rowname,niv_soc, pesos, tot_pesos, value,seleccion) |>
      tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
      select(-rowname) %>%
      group_by(niv_soc) |>
      summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
                tot_pesos = unique(tot_pesos) ) %>%
      tidyr::pivot_longer(-c(tot_pesos,niv_soc), names_to = "respuesta",values_to = "value") %>%
      #group_by(niv_soc) |>
      mutate(coef = value/tot_pesos,
             respuesta = forcats::fct_reorder(.f = respuesta,
                                              .x = coef,
                                              .fun = max))|>
      mutate(variable = niv_soc,
             cruce = "niv_soc",
             se = 0) |>
      rename(   "valor_cruce" = "niv_soc") |>
      select(-c(tot_pesos,value))

  ) |>
  mutate(pregunta = (diccionario |>
           filter(grepl("problema_chile_O",llave)) |>
           distinct(pregunta) |> pull())
           )





######
# cualidades
######

base_cruces_gral_cualidades<-
  bd_respuestas_efectivas |>
  filter(sexo != "Ns/Nc (No leer)") |>
  #mutate(sexo = ifelse(sexo == "Ns/Nc (No leer)","Ns/Nc",sexo )) |>
  tibble::rownames_to_column() %>%
  as_tibble() |>
  select(rowname,sexo ,contains("cualidades_valora_candidato_O"),pesos) |>
  #group_by(sexo) |>
  mutate(tot_pesos = sum(pesos)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = !c(rowname,pesos,sexo,tot_pesos)) |>
  filter(!is.na(value)) %>%
  mutate(seleccion = 1) %>%
  select(-name) %>%
  distinct(rowname,sexo, pesos, tot_pesos, value,seleccion) |>
  tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
  select(-rowname) %>%
  group_by(sexo) |>
  summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
            tot_pesos = unique(tot_pesos) ) %>%
  tidyr::pivot_longer(-c(tot_pesos,sexo), names_to = "respuesta",values_to = "value") %>%
  #group_by(sexo) |>
  mutate(coef = value/tot_pesos,
         respuesta = forcats::fct_reorder(.f = respuesta,
                                          .x = coef,
                                          .fun = max))|>
  mutate(variable = sexo,
         cruce = "sexo",
         se = 0) |>
  rename(   "valor_cruce" = "sexo") |>
  select(-c(tot_pesos,value)) |>
  # calculo por edad
  bind_rows(

    bd_respuestas_efectivas |>
      filter(grupo_edad != "Ns/Nc (No leer)") |>
      #mutate(grupo_edad = ifelse(grupo_edad == "Ns/Nc (No leer)","Ns/Nc",grupo_edad )) |>
      tibble::rownames_to_column() %>%
      as_tibble() |>
      select(rowname,grupo_edad ,contains("cualidades_valora_candidato_O"),pesos) |>
      #group_by(grupo_edad) |>
      mutate(tot_pesos = sum(pesos)) |>
      ungroup() |>
      tidyr::pivot_longer(cols = !c(rowname,pesos,grupo_edad,tot_pesos)) |>
      filter(!is.na(value)) %>%
      mutate(seleccion = 1) %>%
      select(-name) %>%
      distinct(rowname,grupo_edad, pesos, tot_pesos, value,seleccion) |>
      tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
      select(-rowname) %>%
      group_by(grupo_edad) |>
      summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
                tot_pesos = unique(tot_pesos) ) %>%
      tidyr::pivot_longer(-c(tot_pesos,grupo_edad), names_to = "respuesta",values_to = "value") %>%
      #group_by(grupo_edad) |>
      mutate(coef = value/tot_pesos,
             respuesta = forcats::fct_reorder(.f = respuesta,
                                              .x = coef,
                                              .fun = max))|>
      mutate(variable = grupo_edad,
             cruce = "grupo_edad",
             se = 0) |>
      rename(   "valor_cruce" = "grupo_edad") |>
      select(-c(tot_pesos,value))

  ) |>
  # calculo por grup economico
  bind_rows(

    bd_respuestas_efectivas |>
      filter(niv_soc != "Ns/Nc (No leer)") |>
      #mutate(niv_soc = ifelse(niv_soc == "Ns/Nc (No leer)","Ns/Nc",niv_soc )) |>
      tibble::rownames_to_column() %>%
      as_tibble() |>
      select(rowname,niv_soc ,contains("cualidades_valora_candidato_O"),pesos) |>
      #group_by(niv_soc) |>
      mutate(tot_pesos = sum(pesos)) |>
      ungroup() |>
      tidyr::pivot_longer(cols = !c(rowname,pesos,niv_soc,tot_pesos)) |>
      filter(!is.na(value)) %>%
      mutate(seleccion = 1) %>%
      select(-name) %>%
      distinct(rowname,niv_soc, pesos, tot_pesos, value,seleccion) |>
      tidyr::pivot_wider(names_from = value, values_from = seleccion,values_fill = 0)%>%
      select(-rowname) %>%
      group_by(niv_soc) |>
      summarise(across(-c(pesos,tot_pesos),~sum(.x * pesos)),
                tot_pesos = unique(tot_pesos) ) %>%
      tidyr::pivot_longer(-c(tot_pesos,niv_soc), names_to = "respuesta",values_to = "value") %>%
      #group_by(niv_soc) |>
      mutate(coef = value/tot_pesos,
             respuesta = forcats::fct_reorder(.f = respuesta,
                                              .x = coef,
                                              .fun = max))|>
      mutate(variable = niv_soc,
             cruce = "niv_soc",
             se = 0) |>
      rename(   "valor_cruce" = "niv_soc") |>
      select(-c(tot_pesos,value))

  ) |>
  mutate(pregunta = (diccionario |>
                       filter(grepl("cualidades_valora_candidato_O",llave)) |>
                       distinct(pregunta) |> pull())
  )




###################



base_cruces_gral |>
  left_join(diccionario |>
              select(llave,pregunta),
            by=c("variable"="llave")) |>
  bind_rows(base_cruces_gral_problemas) |>
  bind_rows(base_cruces_gral_cualidades) |>
  select(pregunta,variable,cruce,valor_cruce,respuesta,coef,se) |>
  writexl::write_xlsx(path = './data-raw/bd_genericas/bd_cruces_generales.xlsx')










