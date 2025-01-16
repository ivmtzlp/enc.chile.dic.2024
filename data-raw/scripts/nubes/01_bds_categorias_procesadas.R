# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

# Rutas ---------------------------------------------------------------------------------------

link_aprobadas <- "https://docs.google.com/spreadsheets/d/15NOP8mTS-c6RemSPj6NvEcQYuWu_MVGvsEkK0YtvTNQ/edit?usp=sharing"
bot_path <- "../categoriza_bot/data/enc_chile_dic2024/Inputs/"
bot_path_2 <- "../categoriza_bot/data/enc_chile_dic2024/Inputs2/"

# Insumos -------------------------------------------------------------------------------------

path_bd_categorias <-
  "./data-raw/bd_categorias.xlsx"

googledrive::drive_download(file = link_aprobadas,
                            path = path_bd_categorias,
                            overwrite = T)
2

bd_categorias_raw <-
  readxl::read_xlsx(path = path_bd_categorias, skip = 1) |>
  rename(razon_opinion_bachelet_positiva = razon_opinon_bachelet_positiva,razon_opinion_bachelet_negativa=razon_opinon_bachelet_negativa,
         razon_opinion_parisi_negativa=razon_opinion_parisi_nehativa)


# Rutina --------------------------------------------------------------------------------------

variables <- c("prioridad_gob_maru",
               "nombre_prox_gb")

# "razonopinion_per1_cruz_buena"

for(i in seq.int(from = 1, to = length(variables), by = 1)) {

  categorias <-
    bd_categorias_raw |>
    select(variables[i]) |>
    na.omit()

  writexl::write_xlsx(x = list("encuesta" = enc_chih$muestra$diseno$variables |>
                                 as_tibble() |>
                                 select(id = SbjNum, variables[i]) |>
                                 na.omit(variables[i]),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    variables[i],
                                    ".xlsx"))
}



###
# Bachelet
###


# Razon opinion positiva
categorias <-
  bd_categorias_raw |>
  select(razon_opinion_bachelet_positiva) |>
  na.omit()

writexl::write_xlsx(x = list("encuesta" = bd_respuestas_efectivas |>
                               as_tibble() |>
                               filter(opinion_bachelet %in% c("Positiva")) |>
                               select(id = SbjNum,
                                      razon_opinion_bachelet_positiva = razon_opinon_bachelet) |>
                               na.omit(razon_opinion_bachelet_positiva),
                             "categorias" = categorias),
                    path = paste0(bot_path,
                                  "razon_opinion_bachelet_positiva",
                                  ".xlsx"))


# Razon opinion negativa

categorias <-
  bd_categorias_raw |>
  select(razon_opinion_bachelet_negativa) |>
  na.omit()

writexl::write_xlsx(x = list("encuesta" =  bd_respuestas_efectivas |>
                               as_tibble() |>
                               filter(opinion_bachelet %in% c("Negativa")) |>
                               select(id = SbjNum,
                                      razon_opinion_bachelet_negativa = razon_opinon_bachelet) |>
                               na.omit(razon_opinion_bachelet_negativa),
                             "categorias" = categorias),
                    path = paste0(bot_path,
                                  "razon_opinion_bachelet_negativa",
                                  ".xlsx"))

#################3
asepctos_nubes<-
diccionario |>
  filter(grepl("opinion",llave) ) |>
  filter(!grepl("razon",llave) ) |>
  select(llave) |>
  mutate(aspecto = gsub('opinion_',"",llave)) |>
  filter(!aspecto %in%  c("bachelet","parisi","kast","kaiser","primarias") ) |>
  pull(aspecto)

#################3

for(aspecto in asepctos_nubes){


  razon_op_buena_cat<- paste0("razon_opinion_",aspecto,"_positiva")
  razon_op_mala_cat<- paste0("razon_opinion_",aspecto,"_negativa")
  razon_op_personaje <- paste0("razon_opinion_",aspecto)
  op_personaje <- paste0("opinion_",aspecto)


  # Razon opinion positiva
  categorias <-
    bd_categorias_raw |>
    select(!!rlang::sym(razon_op_buena_cat)) |>
    na.omit()

  writexl::write_xlsx(x = list("encuesta" = bd_respuestas_efectivas |>
                                 as_tibble() |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Positiva")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_buena_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_buena_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    razon_op_buena_cat,
                                    ".xlsx"))


  # Razon opinion negativa

  categorias <-
    bd_categorias_raw |>
    select(!!rlang::sym(razon_op_mala_cat)) |>
    na.omit()

  writexl::write_xlsx(x = list("encuesta" =  bd_respuestas_efectivas |>
                                 as_tibble() |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Negativa")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_mala_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_mala_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    razon_op_mala_cat,
                                    ".xlsx"))

}

################################################################################
#faltantes V1
#####################################################################
#################3
asepctos_nubes<-
  diccionario |>
  filter(grepl("opinion",llave) ) |>
  filter(!grepl("razon",llave) ) |>
  select(llave) |>
  mutate(aspecto = gsub('opinion_',"",llave)) |>
  filter(aspecto %in%  c("parisi","kast","kaiser") ) |>
  pull(aspecto)

#################3

for(aspecto in asepctos_nubes){


  razon_op_buena_cat<- paste0("razon_opinion_",aspecto,"_positiva")
  razon_op_mala_cat<- paste0("razon_opinion_",aspecto,"_negativa")
  razon_op_personaje <- paste0("razon_opinion_",aspecto)
  op_personaje <- paste0("opinion_",aspecto)


  # Razon opinion positiva
  categorias <-
    bd_categorias_raw |>
    select(!!rlang::sym(razon_op_buena_cat)) |>
    na.omit()

  writexl::write_xlsx(x = list("encuesta" = bd_respuestas_efectivas |>
                                 as_tibble() |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Positiva")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_buena_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_buena_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path_2,
                                    razon_op_buena_cat,
                                    ".xlsx"))


  # Razon opinion negativa

  categorias <-
    bd_categorias_raw |>
    select(!!rlang::sym(razon_op_mala_cat)) |>
    na.omit()

  writexl::write_xlsx(x = list("encuesta" =  bd_respuestas_efectivas |>
                                 as_tibble() |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Negativa")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_mala_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_mala_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path_2,
                                    razon_op_mala_cat,
                                    ".xlsx"))

}

variables <-
  diccionario |>
  filter(grepl("describe_",llave) ) |>
  select(llave) |>
  mutate(aspecto = gsub('describe_',"",llave)) |>
  filter(aspecto %in%  c("bachelet","winter","vodanovic","ominami") ) |>
  pull(aspecto)

variables <- paste("describe_",variables,sep = "")



for(i in seq.int(from = 1, to = length(variables), by = 1)) {

  categorias <-
    bd_categorias_raw |>
    select(variables[i]) |>
    na.omit()

  writexl::write_xlsx(x = list("encuesta" = bd_respuestas_efectivas |>
                                 as_tibble() |>
                                 select(id = SbjNum, variables[i]) |>
                                 na.omit(variables[i]),
                               "categorias" = categorias),
                      path = paste0(bot_path_2,
                                    variables[i],
                                    ".xlsx"))
}
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


############
# Aprobacion
############



# Razon opinion aprueba

categorias <-
  bd_categorias_raw |>
  select(razon_Maru_aprueba) |>
  na.omit()

writexl::write_xlsx(x = list("encuesta" = enc_chih$muestra$diseno$variables |>
                               as_tibble() |>
                               filter(aprueba_gb %in% c("Aprueba mucho")) |>
                               select(id = SbjNum,
                                      razon_maru_aprueba = razon_Maru) |>
                               na.omit(razon_maru_aprueba),
                             "categorias" = categorias),
                    path = paste0(bot_path,
                                  "razon_maru_aprueba",
                                  ".xlsx"))


# Razon opinion desaprueba

categorias <-
  bd_categorias_raw |>
  select(razon_Maru_desaprueba) |>
  na.omit()

writexl::write_xlsx(x = list("encuesta" = enc_chih$muestra$diseno$variables |>
                               as_tibble() |>
                               filter(aprueba_gb %in% c("Desaprueba mucho")) |>
                               select(id = SbjNum,
                                      razon_maru_desaprueba = razon_Maru) |>
                               na.omit(razon_maru_desaprueba),
                             "categorias" = categorias),
                    path = paste0(bot_path,
                                  "razon_maru_desaprueba",
                                  ".xlsx"))




