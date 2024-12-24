

graficar_intervalo_numerica <- function(bd, escala = c(0, 10), point_size = 1, text_point_size = 8){
  g <-
    bd %>%
    ggplot(aes(y = media, x = stats::reorder(str_wrap(tema, 40), media))) +
    geom_pointrange(aes(ymin = inf, ymax = sup), color = "#850D2D", size = point_size)
  if(escala[2] == 1) {
    g <-
      g +
      geom_text(aes(label = scales::percent(x = media, accuracy = 1.0)),
                nudge_x = .3, size = text_point_size)
  } else {
    g <-
      g +
      geom_text(aes(label = round(media, digits = 2)),
                nudge_x = .3, size = text_point_size)
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
