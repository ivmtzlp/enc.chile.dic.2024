resultados_cali_vec |>
  purrr::map_df(.f= ~{
    as.data.frame(eval(rlang::sym(.x))) |>
      mutate(aspecto = .x)
  })

!!rlang::sym("resultados_cali_delincuencia")



