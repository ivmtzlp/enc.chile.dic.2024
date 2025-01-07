

# dir_bd<- "H:/Shared drives/Morant Consultores/Clientes/MarcoEnriquezOminami_Chile/Encuesta/Insumos/Densidad poblacional por Divisines Censales/Comuna"
#
# data_comuna<- sf::read_sf(paste0(dir_bd,"/Comuna_Densid_Superficie.shp"))


# data_comuna |>
#   as_tibble() |>
#   group_by(NOM_COMUNA) |>
#     summarise(across(.cols = c(T_HOM_R:T_POB ),.fns = sum)) |>
#     writexl::write_xlsx(path = './data-raw/poblacion_censo_chile.xlsx')



####

#comunas[!(comunas %in%  comunas_dentro) ]

# encuestar:::estandarizar_texto()
#
# COQUIMBO
# LA SERENA
# VALPARAÍSO
# VIÑA DEL MAR
comunas_encuesta <- bd_respuestas_efectivas |>
  distinct(comuna) |>
  pull(comuna)

bd_respuestas_efectivas <-
bd_respuestas_efectivas |>
  mutate(sexo = sample(c('Hombres',"Mujeres"),size =2125,replace = T ))


#### Sexo

comuna_pbl <- readxl::read_xlsx('./data-raw/poblacion_censo_chile.xlsx')


#relacion_genero_graf<-
  comuna_pbl |>
  mutate(NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                                  "Latin-ASCII")) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>
  summarise(across(.cols = c(T_HOM:T_MUJ),.fns = sum)) |>
  mutate(tipo = "real") |>
  rename(Hombres = T_HOM, Mujeres  = T_MUJ) |>
  tidyr::pivot_longer(cols = -tipo,names_to = 'sexo',values_to = 'poblacion') |>
  mutate(media = poblacion/sum(poblacion)) |>
  bind_rows(bd_respuestas_efectivas |>
              count(sexo,name = "poblacion") |>
              mutate(media = poblacion/sum(poblacion),
                     tipo = "encuesta")
              ) |>
    ggplot(aes(x = tipo,y = media,group = sexo, colour = sexo)) +
    geom_point(size = 3)+
    geom_line(linewidth = 1.5)+
    tema_morant()+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    theme(legend.position = "right",
          legend.text = element_text(size = 17)   )+
    guides(color = guide_legend(ncol  = 1))



########################

#### Edad

# dir_bd_tot<- "H:/Shared drives/Morant Consultores/Clientes/MarcoEnriquezOminami_Chile/Encuesta/Insumos/Personas Censal/Personas/Microdato_Censo2017-Personas.csv"
#
# dir_bd_micro <- "H:/Shared drives/Morant Consultores/Clientes/MarcoEnriquezOminami_Chile/Encuesta/Insumos/Personas Censal/"
#
# censo_total <- read.csv(dir_bd_tot,sep = ';')
#
# comunas_micro <- read.csv(paste0(dir_bd_micro,'etiquetas_persona_comuna_16r.csv'),sep = ';')
#
# comunas_micro <-comunas_micro |>
#   as_tibble()
#
# censo_total <- censo_total |>
#   as_tibble()
#
# censo_total |> names()
#
#
#
# censo_total |>
# select(REGION:COMUNA,P08,P09) |>
#   rename(sexo=P08, edad=P09) |>
#   mutate(sexo_nom = case_match(sexo,
#                            1~"Hombre",
#                            2~"Mujer",
#                            .default = NA)) |> #distinct(sexo_nom)
#   left_join(comunas_micro |>
#               rename(COMUNA=valor,NOM_COMUNA = glosa),
#             by = 'COMUNA') |>
#   write.csv('./data-raw/poblacion_censo_sexo_edad_chile.csv')

#rm(censo_tot_sexo_edad)

censo_tot_sexo_edad<-
  read.csv('./data-raw/poblacion_censo_sexo_edad_chile.csv') |>
  as_tibble()


# censo_tot_sexo_edad |>
#   count(COMUNA,NOM_COMUNA,sexo,sexo_nom,edad,name = "poblacion")|>
#   write.csv('./data-raw/poblacion_censo_sexo_edad__comuna_chile.csv')

censo_tot_comu_sexo_edad<-
  read.csv('./data-raw/poblacion_censo_sexo_edad__comuna_chile.csv') |>
  as_tibble()


relacion_sexo<-
censo_tot_comu_sexo_edad |>
  mutate(NOM_COMUNA = stringr::str_to_upper(NOM_COMUNA),
         NOM_COMUNA = stringi::stri_trans_general(NOM_COMUNA,
                                     "Latin-ASCII")) |>
  filter(NOM_COMUNA %in% comunas_encuesta) |>
  group_by(rango_edad =  case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                   edad >= 30 & edad <= 39 ~ "30-39",
                                   edad >= 40 & edad <= 49 ~ "40-49",
                                   edad >= 50 & edad <= 59 ~ "50-59",
                                   edad >= 60 & edad <= 64 ~ "60-64",
                                   edad >= 65 ~ "65+",
                                   T ~ NA)
  )|>
  summarise(n = sum(poblacion)) |>
  ungroup() |>
  mutate(real = n/sum(n)) |>
  #rename(respuesta = rango_edad) |>
  filter(!is.na(rango_edad)) |>
  left_join(bd_respuestas_efectivas |>
              group_by(rango_edad =  case_when(edad >= 18 & edad <= 29 ~ "18-29",
                                               edad >= 30 & edad <= 39 ~ "30-39",
                                               edad >= 40 & edad <= 49 ~ "40-49",
                                               edad >= 50 & edad <= 59 ~ "50-59",
                                               edad >= 60 & edad <= 64 ~ "60-64",
                                               edad >= 65 ~ "65+",
                                               T ~ NA)
              )|>
              count(rango_edad,name = "n_encuesta") |>
              ungroup() |>
              mutate(encuesta = n_encuesta/sum(n_encuesta)) ,
              #rename(respuesta = rango_edad),
            by = 'rango_edad') |>
  select(rango_edad,real,encuesta) |>
  tidyr::pivot_longer(cols = -rango_edad,names_to = "tipo",values_to = "media" ) |>
  ggplot(aes(x = tipo,y = media,group = rango_edad, colour = rango_edad)) +
  geom_point(size = 3)+
  geom_line(linewidth = 1.5)+
  tema_morant()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(legend.position = "right",
        legend.text = element_text(size = 17)   )+
  guides(color = guide_legend(ncol  = 1))






