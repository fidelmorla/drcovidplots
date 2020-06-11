#' @title Positiveness by region from COVID19 in the Dominican Republic
#' @aliases g_positive_region
#' @description This function graphs the positive ratio (Positive cases / Tests)
#' from COVID19 in the Dominican Republic ordered by provinces.
#' @usage g_positive_region(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Graph of the the positive ratio by region and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_positive_region()
#' g_positive_region(saveplot = TRUE, savepng = FALSE)
#' @name g_positive_region

g_positive_region <- function(saveplot = FALSE,
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

  df_pos_reg <-
    data_province %>%
    filter(date == max(date)) %>%
    mutate(Reg = case_when(Province %in% Metropolitan ~ 'Metropolitan',
                           Province %in% North ~ 'North',
                           Province %in% South ~ 'South',
                           Province %in% East ~ 'East',
                           Province %in% NE ~ 'NOESP')) %>%
    group_by(Reg) %>%
    summarise(Pos = 100 * sum(Cases)/sum(Tests)) %>%
    filter(Reg != 'NOESP') %>%
    mutate(rank = rank(Pos, ties.method = "first"))

  heatcol_pos_reg <-
    # c('#0b559e', #azul
    #   '#b50012', #rojo
    #   '#03996f', #verde
    #   '#805511') #marron
    sequential_hcl(n = 4,
                   palette = "RedOr",
                   power = 0.5,
                   l = 30,
                   c = 225,
                   c1 = 150)


  rep_actual <- data_cum$Reports %>% max(na.rm = TRUE)

  max_pos_reg <-
    df_pos_reg %>%
    summarise(max_pos_reg = round(max(Pos))) %>%
    select(max_pos_reg) %>%
    as.integer() %>%
    round(digits = -1) %>%
    sum(10)

  total_pos <- round(100 * sum(df_pos_prov$Cases)/sum(df_pos_prov$Tests),
                     digits = 2)
  lab_pos_reg <-
    labs(title = "DR: Positive ratio from COVID-19 by region",
         subtitle  =  paste0("Country's positive ratio = ",
                             total_pos,
                             "%"),
         caption  = paste0("Source: @fidelmorla with information from special bulletin #",
                           rep_actual,
                           " of @SaludPublicaRD"),
         x = "",
         y = "%"
    )


g_pos_reg <-
  df_pos_reg %>%
  ggplot(aes(x = reorder(Reg, rank),
             y = Pos,
             fill = reorder(Reg, -rank),
             col = reorder(Reg, -rank))) +
    geom_col() +
    scale_y_continuous(breaks = c(seq(0,max_pos_reg, max_pos_reg / 4)),
                       limits = c(0,max_pos_reg)) +
    geom_text(check_overlap = TRUE,
              size = 4.5,
              hjust = -0.25,
              show.legend = FALSE,
              aes(label = sprintf("%.2f", Pos))) +
    coord_flip() +
    scale_fill_manual(values = heatcol_pos_reg) +
    scale_color_manual(values = heatcol_pos_reg) +
    lab_pos_reg +
    t6 +
    theme(axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(color = rev(heatcol_pos_reg)))

  print(g_pos_reg)

  if (saveplot == TRUE){
    assign('g_pos_reg', g_pos_reg, envir = .GlobalEnv)
  }

  if (savepng == TRUE){

    ggsave(filename = "positive_region.png",
           plot = g_pos_reg,
           device = "png",
           width = 18.333333333333332 / 1.5,
           height = 10.466666666666667 / 1.5,
           units = "in")
  }

}

