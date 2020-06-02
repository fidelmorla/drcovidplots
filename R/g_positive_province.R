#' @title Positiveness by province from COVID19 in the Dominican Republic
#' @aliases g_positive_province
#' @description This function graphs the positive ratio (Positive cases / Tests)
#' from COVID19 in the Dominican Republic ordered by provinces.
#' @usage g_positive_province()
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Graph of the the positive ratio by provinces and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_positive_province()
#' g_positive_province(saveplot = FALSE, savepng = FALSE)
#' @name g_positive_province

g_positive_province <- function(saveplot = FALSE,
                                 savepng = FALSE){

  if (exists('data_province') == FALSE) {
    stop("data_province is not present, run load_data_covid_dr()")
  }

  if (exists('data_cum') == FALSE) {
    stop("data_cum is not present, run load_data_covid_dr()")
  }
  if (exists('t3') == FALSE) {
    stop("Themes are not present, run load_themes()")
  }

  df_pos_prov <-
    data_province  %>%
    mutate(Tests = as.numeric(Tests)) %>%
    filter(date == max(date),
           Province != 'NOESP') %>%
    mutate(Pos = 100 * Cases / Tests,
           rank = rank(Pos, ties.method = "first")) %>%
    drop_na()

  rep_actual <-
    data_cum$Reports %>% max(na.rm = TRUE)

  max_pos_p <-
    df_pos_prov %>%
    summarise(max_pos_prov = round(max(Pos))) %>%
    select(max_pos_prov) %>%
    as.integer() %>%
    round(digits = -1) %>%
    sum(10)

  total_pos <- round(100 * sum(df_pos_prov$Cases)/sum(df_pos_prov$Tests),
                     digits = 2)

  lab_pos_prov <-
    labs(title = "DR: Positive ratio from COVID-19 by province",
         subtitle  =  paste0("Country's positive ratio = ",
                             total_pos,
                             "%"),
         caption  = paste0("Source: @fidelmorla with information from special bulletin #",
                           rep_actual,
                           " of @SaludPublicaRD"),
         x = "",
         y = "%"
    )

  heatcol_pos <- sequential_hcl(n = 32,
                                palette = "OrRd",
                                power = 0.5,
                                l = 30,
                                c = 225,
                                c1 = 150)

  g_pos_prov <-
    df_pos_prov %>%
    ggplot(aes(x = reorder(Province, rank, order = TRUE),
               y = Pos,
               fill = reorder(Province, -rank, order = TRUE),
               col = reorder(Province, -rank, order = TRUE))) +
    geom_col() +
    scale_y_continuous(breaks = c(seq(0,
                                      max_pos_p,
                                      max_pos_p / 4)),
                       limits = c(0,
                                  max_pos_p)) +
    geom_text(aes(size = 16,
                  label = round(Pos,
                                digits = 1)),
              hjust = -0.25) +
    scale_fill_manual(values = heatcol_pos) +
    scale_color_manual(values = heatcol_pos) +
    coord_flip() +
    lab_pos_prov +
    t6 +
    theme(axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(angle = 0),
          axis.text.y = element_text(color = rev(heatcol_pos)))

  print(g_pos_prov)

  if (saveplot == TRUE){
    assign('g_pos_prov', g_pos_prov, envir = .GlobalEnv)
  }

  if (savepng == TRUE){

    ggsave(filename = "positive_province.png",
           plot = g_pos_prov,
           device = "png",
           width = 18.333333333333332 / 1.5,
           height = 10.466666666666667 / 1.5,
           units = "in")
}

}
