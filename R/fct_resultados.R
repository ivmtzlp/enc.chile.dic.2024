#' resultados
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
calcular_resultados_calificacion <- function(bd_entrevistas_efectivas, variable){

  bd_resultados <-
    bd_entrevistas_efectivas |>
    as_tibble() |>
    count(!!rlang::sym(variable), sort = TRUE) |>
    tidyr::complete(!!rlang::sym(variable) := c(as.character(seq.int(from = 1,
                                                                     to = 7,
                                                                     by = 1)),
                                                "Ns/Nc"),
                    fill = list(n = 0)) |>
    mutate(pct = n/sum(n))

  pct_nsnc <-
    bd_resultados |>
    filter(!!rlang::sym(variable) == "Ns/Nc") |>
    pull(pct)

  media <-
    bd_resultados |>
    filter(!!rlang::sym(variable) != "Ns/Nc") |>
    summarise(media = sum(as.numeric(!!rlang::sym(variable)) * n / sum(n))) |>
    pull()

  return(list(
    pct_nsnc = pct_nsnc,
    media = media
  ))

}
