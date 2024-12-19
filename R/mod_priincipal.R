#' priincipal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import ggplot2 sf leaflet highcharter bslib dplyr
#' @importFrom shiny NS tagList
mod_priincipal_ui <- function(id){
  ns <- NS(id)
  bslib::card(
    full_screen = T,
    layout_sidebar(
      sidebar = sidebar(
        title = "Menú",
        open = TRUE,
        id = "control_mapa",
        width = "300px",
        selectInput(inputId = ns("comuna"),
                    label = h2("Comuna"),
                    choices = c("Todas",
                                sort(unique(shp_comuna$COMUNA))),
                    selected = "Todas",
                    width = "400%"),
        actionButton(
          inputId = ns("filtrar_comuna"),
          label = "Buscar"),
        selectInput(inputId = ns("encuestador"),
                    label = h2("Encuestador"),
                    choices = c("Todos",
                                sort(unique(shp_respuestas_efectivas$Srvyr))),
                    selected = "Todos",
                    width = "400%"),
        actionButton(
          inputId = ns("mostrar_encuestador"),
          label = "Mostrar"),
      ),
      bslib::accordion(
        open = c("Mapa principal"),
        bslib::accordion_panel(
          title = "Comunas",
          value = "Mapa principal",
          bslib::card_body(
            shinycssloaders::withSpinner(leafletOutput(outputId = ns("mapa_principal"),
                                                       height = "800px")))
        )
      )
    )
  )
}

#' priincipal Server Functions
#'
#' @noRd
mod_priincipal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shp_comuna_react <- eventReactive(input$filtrar_comuna, {
      shp_comuna %>%
        {
          if(input$comuna != "Todas"){
            filter(.data = .,
                   COMUNA == input$comuna)
          } else {
            .
          }
        }
    })

    bd_respuestas_efectivas_react <- eventReactive(input$encuestador, {
      shp_respuestas_efectivas%>%
        {
          if(input$encuestador != "Todos"){
            filter(.data = .,
                   Srvyr  == input$encuestador)
          } else {
            .
          }
        }
    })

    output$mapa_principal <- renderLeaflet({

      map <-
        leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=r&x={x}&y={y}&z={z}",
                 attribution = '© Google') |>
        addPolygons(data = shp_comuna,
                    stroke = TRUE,
                    weight = 2,
                    fillOpacity = 0.2,
                    group = "comunas",
                    popup = ~ glue::glue("<span style='font-size:16px;'>Comuna: {COMUNA}</span>"))

      return(map)
    })

    proxy_mapa_principal <- leafletProxy("mapa_principal")

    comuna_actual <- reactiveVal(value = "")

    observeEvent(input$filtrar_comuna, {
      if(input$comuna != comuna_actual()){

        bbox <-
          shp_comuna_react() |>
          sf::st_bbox()

        proxy_mapa_principal %>%
          flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])

      }
    }
    )

    mostrar_encuestador <- reactiveVal(value = "Todos")

    observeEvent(input$mostrar_encuestador, {

      proxy_mapa_principal |>
        clearGroup("Efectivas") %>%
        addCircleMarkers(data = bd_respuestas_efectivas_react(),
                         opacity = 1,
                         radius = 10,
                         fillOpacity = 1,
                         stroke = F,
                         popup = ~ glue::glue("<span style='font-size:16px;'>Encuestador: {Srvyr} <br>
                                              Fecha: {Date} <br>
                                              Comuna: {comuna} <br>
                                              Manzana: {manzana} </span>"),
                         group = "Efectivas")

    }
    )

  })
}

## To be copied in the UI
# mod_priincipal_ui("priincipal_1")

## To be copied in the server
# mod_priincipal_server("priincipal_1")
