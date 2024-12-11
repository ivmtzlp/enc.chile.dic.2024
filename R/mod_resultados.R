#' resultados UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_resultados_ui <- function(id){
  ns <- NS(id)
  bslib::card(
    full_screen = T,
    bslib::accordion(
      shinycssloaders::withSpinner(plotOutput(ns("resultado_01"))),
      shinycssloaders::withSpinner(plotOutput(ns("resultado_02")))
    )
  )
}

#' resultados Server Functions
#'
#' @noRd
mod_resultados_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$resultado_01 <- renderPlot({

      bd_respuestas_efectivas |>
        as_tibble() |>
        count(voto_pr) |>
        na.omit() |>
        mutate(media = n/sum(n)) |>
        rename(respuesta = voto_pr) |>
        graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
        tema_morant() +
        theme(axis.text.x = element_text(size = 16),
              plot.caption = element_text(size = 16)) +
        labs(caption = "Si las elecciones presidenciales fueran el próximo domingo, ¿por quién votarías?")

    })

    output$resultado_02 <- renderPlot({

      bd_respuestas_efectivas |>
        as_tibble() |>
        count(voto2_pr) |>
        na.omit() |>
        mutate(media = n/sum(n)) |>
        rename(respuesta = voto2_pr) |>
        graficar_barras(salto = 35, porcentajes_fuera = TRUE, desplazar_porcentajes = 0.01) +
        tema_morant() +
        theme(axis.text.x = element_text(size = 16)) +
        labs(caption = "Si ese candidato no participara en las elecciones, ¿por cuál otro candidato votaría?") +
        theme(axis.text.x = element_text(size = 16),
              plot.caption = element_text(size = 16))

    })

  })
}

## To be copied in the UI
# mod_resultados_ui("resultados_1")

## To be copied in the server
# mod_resultados_server("resultados_1")
