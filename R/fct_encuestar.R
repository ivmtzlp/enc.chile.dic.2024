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

graficar_barras <- function(bd,
                            salto = 20,
                            porcentajes_fuera = F,
                            desplazar_porcentajes = 0,
                            orden_respuestas = NA,
                            text_size = 8){

  g <-
    bd %>%
    {
      if(length(orden_respuestas) == 1) {
        ggplot(data = .,
               aes(x = forcats::fct_reorder(stringr::str_wrap(respuesta, salto), media),
                   y  = media,
                   fill = respuesta))
      } else {
        ggplot(data = .,
               aes(x = factor(stringr::str_wrap(respuesta, salto), levels = stringr::str_wrap(orden_respuestas, salto)),
                   y  = media,
                   fill = respuesta))
      }
    } +
    ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"), color = "transparent", alpha = 0.8, width = 0.45)
  if (porcentajes_fuera == F) {
    g <-
      g +
      ggfittext::geom_bar_text(aes(label = scales::percent(media, accuracy = 1)), contrast = T)
  }
  if (porcentajes_fuera == T) {
    g <-
      g +
      geom_text(aes(label = scales::percent(media, accuracy = 1)), nudge_y = desplazar_porcentajes, size = text_size)
  }
  g <-
    g +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  return(g)
}

formato_archivo = function(nombre, extension, tolerancia = 10) {
  paste0(nombre,
         "_",
         gsub(pattern = "-", replacement = "", x = Sys.Date()),
         "_",
         format(Sys.time(), "%H"),
         as.character(floor(as.integer(format(Sys.time(), "%M")) / tolerancia) * tolerancia),
         "h.",
         extension)
}

graficar_gauge <- function(bd, color_principal, color_secundario = "gray80", escala, size_text_pct){
  g <-
    bd %>%
    ggplot() +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = 0, ymax = media),
              fill = color_principal,  color = "white", alpha= .95) +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = media, ymax = escala[2]),
              fill = color_secundario, color = "white")
  if(escala[2] == 1) {
    g <-
      g +
      geom_text(aes(x = 0, y = 0.5, label = scales::percent(x = media, accuracy = 1.)),
                size = size_text_pct, family = "Poppins", nudge_y = 0.25)
  }
  else {
    g <-
      g +
      geom_text(aes(x = 0, y = 0.5, label = scales::comma(x = media, accuracy = 1.1)),
                size = size_text_pct, family = "Poppins", nudge_y = 0.25)
  }
  g <-
    g +
    scale_fill_manual(values = c("#1DCDBC", "#38C6F4")) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, escala[2])) +
    xlab("") +
    ylab("") +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          text = element_text(size = 15, family = "Poppins"))
  return(g)
}

calcular_tasa_rechazo <- function(bd_respuestas_efectivas, por_usuario = FALSE){
  bd_respuestas_efectivas %>%
    {
      if(!por_usuario) {
        summarise(.data = .,
                  rechazo = (sum(as.integer(intento_efectivo)) - n())/sum(as.integer(intento_efectivo)))
      }
      else {
        group_by(.data = .,
                 Srvyr) %>%
          summarise(rechazo = (sum(as.integer(intento_efectivo)) - n())/sum(as.integer(intento_efectivo))) |>
          arrange(desc(rechazo))
      }
    }
}

asignar_colores = function(tb_respuestas, partidos = T){
  tb_respuestas |>
    mutate(color = dplyr::case_when(grepl(pattern = "por MORENA, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "Astiazarán", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pri,
                                    grepl(pattern = "por PAN, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pan,
                                    grepl(pattern = "por Movimiento Ciudadano, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_mc,
                                    grepl(pattern = "MORENA", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "Morena", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "PAN", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pan,
                                    grepl(pattern = "PRI", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pri,
                                    grepl(pattern = "PRD", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_prd,
                                    grepl(pattern = "PT", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pt,
                                    grepl(pattern = "PES", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pes,
                                    grepl(pattern = "Partido Verde", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pvem,
                                    grepl(pattern = "PVEM", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pvem,
                                    grepl(pattern = "Movimiento Ciudadano|MC", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_mc,
                                    grepl(pattern = "Fuerza por", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_fuerzaxmexico,
                                    grepl(pattern = "Otro", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "Ninguno", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "Candidato independiente", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "no registrado", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "No iré a votar", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "Anular", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "No sabe", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "No contesta", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "Ns/Nc", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "Chiapas Unido", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_chisunido,
                                    grepl(pattern = "Encuentro Solidario", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pencsolchis,
                                    grepl(pattern = "Mover a Chiapas", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pmchis,
                                    T ~ color_general)) |>
    pull(color) |>
    purrr::set_names(tb_respuestas |>
                       pull())
}

graficar_lollipops <- function(bd, orden = NULL, limits = c(0., 1.0), width_cats = 15 , size = 2, size_pct = 6) {
  g <-
    bd |>
    ggplot(aes( if(is.null(orden)) x =  reorder(respuesta, pct) else x =  factor(respuesta, levels = orden),
                y = pct)) +
    geom_segment(aes(xend = respuesta,
                     y = 0,
                     yend = pct,
                     color = respuesta),
                 linewidth = size) +
    geom_point(aes(color = respuesta),
               size = size+3) +
    geom_text(aes(label = scales::percent(pct, accuracy = 1.)),
              size = size_pct, hjust = -0.5) +
    coord_flip() +
    scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = width_cats)) +
    scale_y_continuous(labels = scales::percent,
                       limits = limits) +
    theme(plot.background = element_rect(color = "transparent", fill = "transparent"),
          panel.background = element_rect(color = "transparent", fill = "transparent"),
          legend.background = element_rect(color = "transparent", fill = "transparent") )
  return(g)
}

graficar_candidato_opinion <- function(bd, ns_nc, regular,
                                       grupo_positivo,
                                       grupo_negativo,
                                       colores,
                                       color_nsnc,
                                       burbuja,
                                       color_burbuja,
                                       size_burbuja = 8,
                                       caption_opinion = "",
                                       caption_nsnc = "Ns/Nc",
                                       caption_burbuja,
                                       size_text_legend = 12,
                                       size_caption_opinion = 12,
                                       size_caption_nsnc = 14,
                                       size_caption_burbuja,
                                       size_text_cat = 16,
                                       size_pct = 12,
                                       orden_resp,
                                       salto = 200,
                                       tema,
                                       mostrar_nsnc = T,
                                       salto_respuestas = 100,
                                       orden_cat = NULL,
                                       patron_inicial = NULL){

  if(!is.null(ns_nc)){
    bd <- bd %>% group_by(tema) %>% tidyr::complete(respuesta = ns_nc, fill = list(media = 0)) %>% ungroup
  }

  aux <- bd %>% mutate(Regular = if_else(respuesta == regular, "regular1", as.character(respuesta))) %>%
    bind_rows(bd %>% filter(respuesta == regular) %>% mutate(Regular = "regular2", media = -media)) %>%
    mutate(etiqueta = if_else(Regular != "regular2", scales::percent(media,1), ""),
           media = if_else(respuesta %in% grupo_negativo,-1*media,media),
           media = if_else(respuesta == regular, media/2, media)) %>%
    group_by(tema) %>%
    mutate(saldo = sum(as.numeric(!(respuesta %in% c(regular, ns_nc)))*media))

  orden <- aux %>% arrange(saldo) %>% pull(tema) %>% unique %>% na.omit

  if(!is.null(orden_cat)) {

    orden <- aux %>%
      ungroup() |>
      mutate(aspecto = gsub(pattern = paste0(patron_inicial, "_"),
                            replacement = "",
                            x = aspecto)) |>
      distinct(aspecto, tema) |>
      mutate(aspecto = factor(aspecto, levels = orden_cat, ordered = TRUE)) |>
      arrange(desc(aspecto)) |>
      pull() |>
      as.factor()

  }

  if(!all(is.na(burbuja))){
    burbuja <- burbuja %>% mutate(escala = media/max(media), tema = forcats::fct_reorder(tema, media))
    orden <- burbuja$tema %>% levels
    g_burbuja <-
      burbuja %>%
      ggplot(aes(y = tema,
                 x = factor(1))) +
      geom_point(aes(size = escala), color = color_burbuja, shape = 16) +
      geom_text(aes(label = scales::percent(media,1)), hjust = -.5) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = salto)) +
      scale_size_area(max_size = size_burbuja) +
      tema_morant() +
      labs(caption = caption_burbuja) +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line.x = element_blank(),
            plot.caption = element_text(hjust = 0.5, size = size_caption_burbuja))
  }

  g_opinion <-
    aux %>%
    {if(!is.null(ns_nc)) filter(., respuesta!= ns_nc) else .}  %>%
    mutate(respuesta = factor(respuesta, levels = orden_resp)) |>
    graficar_barras_saldo(orden = orden,
                          grupo_positivo = grupo_positivo,
                          grupo_negativo = grupo_negativo,
                          Regular = regular,
                          colores = colores,
                          salto_respuestas = salto_respuestas,
                          salto_tema = salto,
                          caption_opinion = caption_opinion,
                          size_text_cat = size_text_cat,
                          size_pct = size_pct,
                          size_text_legend = size_text_legend,
                          size_caption_opinion = size_caption_opinion)

  if(!is.null(ns_nc)){
    b <- aux %>%
      filter(respuesta == ns_nc) %>%
      ggplot(aes(x = factor(tema, orden),
                 y = media))+
      ggchicklet::geom_chicklet(width =.6, alpha =.9, fill = color_nsnc, color = "transparent") +
      ggfittext::geom_bar_text(aes(label = etiqueta), color = "#2C423F",
                               hjust = -.1) +
      coord_flip() +
      labs(y = NULL, x = NULL, caption = caption_nsnc) +
      scale_y_continuous(n.breaks = 2) +
      tema_morant() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.x = element_blank(), axis.line.x = element_blank(),
            plot.caption = element_text(hjust = 0.5, size = size_caption_nsnc))

    if(!all(is.na(burbuja))){
      if(mostrar_nsnc) {
        final <- g_opinion + g_burbuja + b + patchwork::plot_layout(widths = c(.7, .15, .15), ncol= 3)
      } else {
        final <- g_opinion + g_burbuja + patchwork::plot_layout(widths = c(.7, .15, .15), ncol = 3)
      }
    } else{
      if(mostrar_nsnc) {
        final <- g_opinion + b + patchwork::plot_layout(widths = c(.8, .2))
      } else {
        final <- g_opinion
      }

    }

  } else{
    if(!all(is.na(burbuja))){
      final <- g_opinion + g_burbuja + patchwork::plot_layout(widths = c(.8,.2))
    } else{
      final <- g_opinion
    }
  }
  return(final &
           theme(plot.background = element_rect(color = "transparent", fill = "transparent"),
                 panel.background = element_rect(color = "transparent", fill = "transparent"),
                 legend.background = element_rect(color = "transparent", fill = "transparent")))
}

graficar_barras_saldo <- function(bd, orden, grupo_positivo, grupo_negativo, Regular, colores, salto_respuestas, salto_tema, caption_opinion, size_text_cat = 12, size_pct, size_caption_opinion,size_text_legend = 12, tema = tema_morant()){

  if(!is.na(Regular)) {
    group_levels <- c("regular2", grupo_negativo, "regular1", grupo_positivo)
  } else {
    group_levels <- c(grupo_negativo, grupo_positivo)
  }

  g <-
    bd |>
    ggplot(aes(x  = factor(tema, orden),
               y = media,
               fill = respuesta,
               group = factor(Regular, levels = group_levels))) +
    ggchicklet::geom_chicklet(color = "transparent", width =.6, alpha =.9) +
    ggfittext::geom_fit_text(aes(label = etiqueta),
                             size = size_pct,
                             position = position_stack(.5, reverse = T),
                             vjust = .5,
                             contrast = T,
                             show.legend = F) +
    geom_hline(yintercept = 0, color = "#FFFFFF", size = .6) +
    geom_hline(yintercept = 0, color = "gray", size = .6) +
    coord_flip() +
    scale_fill_manual(values = colores,
                      labels = function(x) stringr::str_wrap(string = x, width = salto_respuestas)) +
    labs(x = NULL,
         y = NULL,
         fill = NULL,
         caption = caption_opinion) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = salto_tema)) +
    lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
    theme_minimal() +
    tema_morant() +
    theme(legend.position = "bottom") +
    theme(axis.text.y = element_text(size = size_text_cat),
          plot.caption = element_text(hjust = 0.5, size = size_caption_opinion),
          legend.key.size = unit(1, units = "cm"),
          legend.text = element_text(size = size_text_legend))
  return(g)
}



graficar_intervalo_numerica <- function(bd, escala = c(0, 10), point_size = 1, text_point_size = 8, nudge_x = .3){
  g <-
    bd %>%
    ggplot(aes(y = media, x = stats::reorder(stringr::str_wrap(tema, 40), media))) +
    geom_pointrange(aes(ymin = inf, ymax = sup), color = "#850D2D", size = point_size)
  if(escala[2] == 1) {
    g <-
      g +
      geom_text(aes(label = scales::percent(x = media, accuracy = 1.0)),
                nudge_x = nudge_x, size = text_point_size)
  } else {
    g <-
      g +
      geom_text(aes(label = round(media, digits = 2)),
                nudge_x = nudge_x, size = text_point_size)
  }
  g <-
    g +
    coord_flip() +
    labs(title = NULL,
         x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(escala[1], escala[2]))
  return(g)
}


# graficar_gauge <- function(bd, color_principal, color_secundario = "gray80", escala, size_text_pct){
#   g <-
#     bd %>%
#     ggplot() +
#     geom_rect(aes(xmin = 2, xmax = 3, ymin = 0, ymax = media),
#               fill = color_principal,  color = "white", alpha= .95) +
#     geom_rect(aes(xmin = 2, xmax = 3, ymin = media, ymax = escala[2]),
#               fill = color_secundario, color = "white")
#   if(escala[2] == 1) {
#     g <-
#       g +
#       geom_text(aes(x = 0, y = 0.5, label = scales::percent(x = media, accuracy = 1.)),
#                 size = size_text_pct, family = "Poppins", nudge_y = 0.25)
#   }
#   else {
#     g <-
#       g +
#       geom_text(aes(x = 0, y = 0.5, label = scales::comma(x = media, accuracy = 1.1)),
#                 size = size_text_pct, family = "Poppins", nudge_y = 0.25)
#   }
#   g <-
#     g +
#     scale_fill_manual(values = c("#1DCDBC", "#38C6F4")) +
#     scale_x_continuous(limits = c(0, NA)) +
#     scale_y_continuous(limits = c(0, escala[2])) +
#     xlab("") +
#     ylab("") +
#     coord_polar(theta = "y") +
#     theme_void() +
#     theme(legend.position = "none",
#           axis.text = element_blank(),
#           text = element_text(size = 15, family = "Poppins"))
#   return(g)
# }





