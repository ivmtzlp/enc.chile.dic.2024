# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

# Rutas ---------------------------------------------------------------------------------------

bot_outputs_path <- "../enc_chile_2024/data-raw/scripts/nubes/resultados_bot"

# Cargar resultados ---------------------------------------------------------------------------

archivos <-
  list.files(bot_outputs_path)

bd_categorias_procesada <- NULL

for(i in seq.int(from = 1, to = length(archivos), by = 1)) {

  if(i == 1) {
    bd_categorias_procesada <-
      readxl::read_xlsx(path = paste0(bot_outputs_path, archivos[i])) %>%
      select(id, !!names(.)[4])
  }
  else {
    bd_categorias_procesada <-
      bd_categorias_procesada |>
      left_join(readxl::read_xlsx(path = paste0(bot_outputs_path, archivos[i])) %>%
                  select(id, !!names(.)[4]), by = "id")


  }
}

print("Se ha cargado la base bd_categorias_procesada")
