



p_map_covid <- function(date = "latest", interactive = FALSE, variable = "Cases") {

  if (exists('data_cum') == FALSE) {
    stop("data_cum is not present, run load_data_covid_dr()")
  }

  date2 <- date

  `%>%` <- magrittr::`%>%`

  load("data/map_provincias.RData")


  if(date == "latest") {
    data <- data_province %>%
      dplyr::filter(date == max(date))
  } else {
    data <- data_province %>%
      dplyr::filter(date == date2)
  }

  if(interactive == FALSE) {

  map_covid <- dplyr::left_join(map_provincias, data, by = c("province_short" = "Province")) %>%
    select(var_toplot = variable, dplyr::everything()) %>%
    mutate(
      CENTROID = purrr::map(geometry, sf::st_centroid),
      COORDS = purrr::map(CENTROID, sf::st_coordinates),
      COORDS_X = purrr::map_dbl(COORDS, 1),
      COORDS_Y = purrr::map_dbl(COORDS, 2),
      label_short = paste0(province_short, ": ", format(var_toplot, big.mark = ","))
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = var_toplot)) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_continuous(
     labels = scales::comma,
     guide = ggplot2::guide_colorbar(barwidth = 15)
    ) +
    ggplot2::theme(
      legend.position = c(0.5, 0.2),
      legend.direction = "horizontal"
    ) +
    ggplot2::labs(fill = variable) +
    ggrepel::geom_label_repel(ggplot2::aes(COORDS_X, COORDS_Y, label = label_short),
                              size = 3, min.segment.length = 0, point.padding = NA)
  }

  return(map_covid)

}




