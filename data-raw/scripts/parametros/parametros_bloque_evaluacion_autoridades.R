

# Aprobacion auntoridades

aprueba_autoridades_vars <- c('aprueba_gobierno_boric','aprueba_ministros')

colores_aprueba_autoridades <-c("Desaprueba mucho"= color_opinion_muyMala,
                                "Desaprueba poco"= color_opinion_mala,
                                "No aprueba ni desaprueba"= color_opinion_regular,
                                "Aprueba poco"= color_opinion_buena,
                                "Aprueba mucho"= color_opinion_muyBuena)
aprueba_autoridades_tit <- "Independiente de su posición política, ¿usted aprueba o desaprueba la forma como (...) está/n conduciendo su gobierno? ... ¿mucho o poco?"


aprueba_autoridades_tit <-
  aprueba_autoridades_tit |>
  stringr::str_wrap(width = 55)
