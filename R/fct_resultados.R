#' resultados
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# calcular_resultados_calificacion <- function(bd_entrevistas_efectivas, variable,max_min = F){
#
#   bd_resultados <-
#     bd_entrevistas_efectivas |>
#     as_tibble() |>
#     count(!!rlang::sym(variable), sort = TRUE) |>
#     tidyr::complete(!!rlang::sym(variable) := c(as.character(seq.int(from = 1,
#                                                                      to = 7,
#                                                                      by = 1)),
#                                                 "Ns/Nc"),
#                     fill = list(n = 0)) |>
#     mutate(pct = n/sum(n))
#
#   pct_nsnc <-
#     bd_resultados |>
#     filter(!!rlang::sym(variable) == "Ns/Nc") |>
#     pull(pct)
#
#   media <-
#     bd_resultados |>
#     filter(!!rlang::sym(variable) != "Ns/Nc") |>
#     summarise(media = sum(as.numeric(!!rlang::sym(variable)) * n / sum(n))) |>
#     pull()
#
#   if(max_min){
#     media <-
#       bd_resultados |>
#       filter(!!rlang::sym(variable) != "Ns/Nc") |>
#       summarise(media = sum(as.numeric(!!rlang::sym(variable)) * n / sum(n))) |>
#       pull()
#
#   }
#
#   return(list(
#     pct_nsnc = pct_nsnc,
#     media = media
#   ))
#
# }


calcular_resultados_calificacion <- function(bd_entrevistas_efectivas, variable, limtes = c(1,7),max_min = F){

  bd_resultados <-
    bd_entrevistas_efectivas |>
    as_tibble() |>
    count(!!rlang::sym(variable), sort = TRUE) |>
    tidyr::complete(!!rlang::sym(variable) := c(as.character(seq.int(from = limtes[1],
                                                                     to = limtes[2],
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

  if(max_min){
    minimo <-
      bd_resultados|>
      filter(n == min(n)) |>
      summarise(min = max( !!rlang::sym(variable) ) ) |>
      pull(min)

    maximo <-
      bd_resultados|>
      filter(n == max(n)) |>
      summarise(max = min( !!rlang::sym(variable) ) ) |>
      pull(max)
  }

  if(!max_min){

    return(list(
      pct_nsnc = pct_nsnc,
      media = media
    ))

  }else{
    return(list(
      pct_nsnc = pct_nsnc,
      media = media,
      minimo = minimo,
      maximo = maximo
    ))
  }

}
