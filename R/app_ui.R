#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_navbar(
      shinyjs::useShinyjs(),
      title = "Encuesta Nacional Chile - Dic 2024",
      bslib::nav_panel(
        title = "Mapa Principal",
        mod_priincipal_ui("priincipal_1")
      ),
      bslib::nav_panel(
        title = "Progreso",
        mod_progreso_ui("progreso_1")
      )
      # bslib::nav_panel(
      #   title = "Resultados",
      #   mod_resultados_ui("resultados_1")
      # )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "enc.chile.dic.2024"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
