#' @title Dominican map with COVID-19 cases by province
#' @aliases g_map_covid
#' @description This function graphs the provinces with the most positive
#'   cases of COVID19 in the DR.
#' @usage g_map_covid(date = "latest", interactive = FALSE, variable = "Cases")
#' @param date Character.
#' @param interactive
#' @param variable
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#'
#' @return Graph the Dominican map with COVID-19 cases by province and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @importFrom scales comma
#' @export
#' @examples
#' g_map_covid()
#' g_map_covid(savepng = TRUE)
#' g_map_covid(date = "latest", variable = "Deaths")
#' g_map_covid(date = "2020-04-05", variable = "Recovered")
#' @name g_map_covid

g_map_covid <- function(date = "latest", variable = "Cases", by_habitants = TRUE) {

  if (exists('data_cum') == FALSE) {
    stop("data objects are missing, run load_data_covid_dr()")
  }

  date2 <- date

  `%>%` <- magrittr::`%>%`

  if(date == "latest") {
    data <- data_province %>%
      dplyr::filter(date == max(date))
  } else {
    data <- data_province %>%
      dplyr::filter(date == date2)
  }


  if (variable == "Cases") {
    scale_fill <- colorspace::scale_fill_continuous_sequential(
      palette = "Blues",
      labels = scales::comma,
      guide = ggplot2::guide_colorbar(barwidth = 15)
    )
  } else if (variable == "Deaths") {
    scale_fill <- colorspace::scale_fill_continuous_sequential(
      palette = "YlOrRd",
      labels = scales::comma,
      guide = ggplot2::guide_colorbar(barwidth = 15))
  } else if (variable == "Recovered") {
    scale_fill <- colorspace::scale_fill_continuous_sequential(
      palette = "PuBuGn",
      labels = scales::comma,
      guide = ggplot2::guide_colorbar(barwidth = 15))
  }



  map_covid <- dplyr::left_join(drcovidplots::map_provincias, data, by = c("province_short" = "Province")) %>%
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
    ggplot2::theme(
      legend.position = c(0.5, 0.2),
      legend.direction = "horizontal"
    ) +
    ggplot2::labs(fill = variable) +
    ggrepel::geom_label_repel(ggplot2::aes(COORDS_X, COORDS_Y, label = label_short),
                              size = 3, min.segment.length = 0, point.padding = NA) +
    scale_fill


  return(map_covid)
}





