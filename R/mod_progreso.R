#' progreso UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_progreso_ui <- function(id){
  ns <- NS(id)
  bslib::card(
    full_screen = T,
    bslib::accordion(
      height = "100%",
      open = c("progreso", "acumulado"),
      shinyWidgets::progressBar(
        id = "enc_hechas",
        value = nrow(shp_entrevistas_efectivas),
        display_pct = T,
        striped = T,
        total = 3000,
        status = "success"),
      bslib::accordion_panel(
        value = "progreso",
        title = "Progreso por Comuna",
        shinycssloaders::withSpinner(plotOutput(ns("progreso_comuna")))
        ),
      bslib::accordion_panel(
        value = "acumulado",
        title = "Avance acumulado",
        shinycssloaders::withSpinner(plotOutput(ns("estimacion")))
      )
    )
  )
}

#' progreso Server Functions
#'
#' @noRd
mod_progreso_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$progreso_comuna <- renderPlot({

      tot_efectivas <-
        shp_entrevistas_efectivas %>%
        as_tibble |>
        count(comuna_mm, sort = TRUE, name = "Efectivas") |>
        tidyr::complete(comuna_mm = unique(bd_cuotas_comuna$comuna),
                        fill = list(Efectivas = 0)) |>
        left_join(bd_cuotas_comuna, by = c("comuna_mm" = "comuna")) |>
        mutate(Faltantes = cuota - Efectivas,
               pct = Efectivas/cuota) |>
        arrange(desc(pct))

      g <-
      tot_efectivas |>
        ggplot(aes(x = reorder(comuna_mm, pct))) +
        geom_col(aes(y = Faltantes), fill = "gray70") +
        geom_col(aes(y = Efectivas), fill = color_general) +
        geom_text(aes(y = Efectivas,
                      label = paste0(scales::comma(Efectivas),
                                     "/",
                                     cuota,
                                     " (",
                                     scales::percent(pct, accuracy = 1.0),
                                     ")")),
                  hjust = -0.1,
                  size = 8) +
        coord_flip() +
        scale_x_discrete(labels = function(x) stringr::str_to_title(string = x)) +
        scale_y_continuous(labels = scales::comma,
                           n.breaks = 6) +
        tema_morant() +
        theme(legend.position = "bottom")

      return(g)

    })

    output$estimacion <- renderPlot({

      hist_efectivas <-
        shp_entrevistas_efectivas %>%
        as_tibble %>%
        count(fecha = lubridate::as_date(Date)) |>
        tidyr::complete(fecha = seq.Date(from = min(lubridate::as_date(shp_entrevistas_efectivas$Date)),
                                         to = max(lubridate::as_date(shp_entrevistas_efectivas$Date)),
                                         by = "day"),
                        fill = list(n = 0)) |>
        rename("tot_hechas" = n) |>
        mutate(acum_hechas = cumsum(tot_hechas))

      dataset_ml <-
        hist_efectivas |>
        transmute(dias_desde_inicio = as.numeric(fecha - min(fecha)),
                  acum_hechas)

      lm <-
        lm(acum_hechas ~ dias_desde_inicio, data = dataset_ml)

      dias_pred <- 90

      dataset_pred <-
        tibble(dias_desde_inicio = seq.int(from = max(dataset_ml$dias_desde_inicio) + 1,
                                           to = max(dataset_ml$dias_desde_inicio) + dias_pred,
                                           by = 1))

      predict <-
        predict(lm, newdata = dataset_pred)

      dataset_pred$prediccion = predict

      meta <- 3000

      bd_hecho_pred <-
        hist_efectivas |>
        tidyr::complete(fecha = seq.Date(from = min(lubridate::as_date(shp_entrevistas_efectivas$Date)),
                                         to = max(lubridate::as_date(shp_entrevistas_efectivas$Date)) + dias_pred,
                                         by = "day"),
                        fill = list(n = 0)) |>
        mutate(dias_desde_inicio = as.numeric(fecha - min(fecha))) |>
        left_join(dataset_ml |>
                    bind_rows(dataset_pred)) |>
        mutate(prediccion = round(x = prediccion, digits = 0),
               control = dplyr::if_else(condition = is.na(prediccion),
                                        true = acum_hechas,
                                        false = prediccion)
        ) |>
        mutate(meta = meta) |>
        filter(control <= meta | row_number() == which(control > meta)[1]) |>
        select(!c(dias_desde_inicio, control, meta)) |>
        tidyr::pivot_longer(cols = !c(fecha),
                            names_to = "Tipo",
                            values_to = "valor") |>
        mutate(grupo = dplyr::if_else(condition = Tipo %in% c("acum_hechas", "prediccion"),
                                      true = "acumulado",
                                      false = "totales")) |>
        mutate(Tipo = case_when(Tipo == "acum_hechas" ~ "Efectivas acumuladas",
                                Tipo == "prediccion" ~ "Estimación acumuladas",
                                Tipo == "tot_hechas" ~ "Efectivas diarias"))
      g <-
        bd_hecho_pred %>%
        ggplot(aes(x = fecha,
                   y = valor,
                   color = Tipo,
                   fill = Tipo)) +
        geom_point(data = . %>%
                     filter(grupo == "acumulado"),
                   size = 3) +
        geom_line(data = bd_hecho_pred %>%
                    filter(grupo == 'acumulado') |>
                    na.omit(),
                  aes(group = grupo),
                  show.legend = FALSE) +
        geom_col(data = . %>%
                   filter(Tipo == "Efectivas diarias")) +
        geom_text(aes(label = valor),
                  vjust = -2,
                  size = 6,
                  show.legend = FALSE) +
        scale_x_date(date_breaks = "1 day", date_labels = "%d\n%b") +
        scale_y_continuous(limits = c(0, meta*1.2),
                           breaks = scales::breaks_pretty(n = 6)) +
        labs(title = paste0("Fecha estimada para cumplir ",
                            scales::comma(meta),
                            " entrevistas: ",
                            format(max(bd_hecho_pred$fecha), "%d de %b"))
        ) +
        tema_morant() +
        theme(legend.position = "bottom")
      return(g)

    })

  })
}

## To be copied in the UI
# mod_progreso_ui("progreso_1")

## To be copied in the server
# mod_progreso_server("progreso_1")
