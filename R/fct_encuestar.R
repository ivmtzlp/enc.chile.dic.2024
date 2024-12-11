#' encuestar
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
obtener_ubicacionEfectiva_surveyToGo = function(bd_respuestas, id, intento_efectivo) {
  bd_respuestas |>
    filter(SbjNum == id) |>
    select(SbjNum,
           paste0("INT",
                  intento_efectivo),
           paste0("GPS_INT",
                  intento_efectivo,
                  "_",
                  c("LA", "LO"
                    # "ALT", "BEAR", "SPEED", "DATE"
                  ))) |>
    mutate(across(.cols = !c(SbjNum), .fns = ~ as.character(.x)),
           intento_efectivo = intento_efectivo) |>
    relocate(intento_efectivo, .after = SbjNum) |>
    rename_with(~ gsub(pattern = as.character(intento_efectivo), replacement = "", x = .),
                .cols = everything())
}

tema_transparente <- function(){
  ggplot2::theme(legend.background = element_rect(color = "transparent", fill = "transparent"),
                 panel.background = element_rect(color = "transparent", fill = "transparent"),
                 plot.background = element_rect(color = "transparent", fill = "transparent"),
                 strip.background = element_rect(color = "transparent", fill = "transparent"))
}

tema_morant <- function(base_family = "Poppins") {
  (ggthemes::theme_foundation(base_size = 15,
                              base_family = base_family) +
     theme(
       line = element_line(colour = "#4C5B61"),
       rect = element_rect(fill = "#FFFFFF", linetype = 0, colour = NA),
       text = element_text(color = "#2C423F"),
       axis.title = element_blank(),
       axis.text = element_text(),
       axis.text.x = element_text(size = 14),
       axis.text.y = element_text(size = 16),
       axis.ticks = element_blank(),
       axis.line.x = element_line(colour = "#E1356D"),
       legend.position = "none",
       legend.direction = "horizontal",
       legend.box = "vertical",
       legend.text = element_text(size = 14),
       panel.grid = element_line(colour = NULL),
       panel.grid.major.y = element_blank(),
       panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
       panel.grid.minor = element_blank(),
       plot.title = element_text(hjust = 0, size = rel(1.1), colour = "#4C5B61"),
       plot.subtitle = element_text(hjust = 0, size = rel(1), face = "bold", colour = "#C5C5C5", family = base_family),
       plot.caption = element_text(size = 14),
       plot.margin = unit(c(1, 1, 1, 1), "lines"),
       strip.text = element_text(colour ="#2C423F")
     ) +
     tema_transparente()
  )
}
