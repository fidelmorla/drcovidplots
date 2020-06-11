#' @title Total number of deaths
#' @aliases g_death
#' @description This function graphs the total number of deaths by COVID19 in the Dominican Republic.
#' @usage g_death(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Graph of the total number of deaths and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @importFrom scales comma
#' @export
#' @examples
#' g_death()
#' g_death(savepng = TRUE)
#' @name g_death

g_death <-
  function(saveplot = FALSE,
           savepng = FALSE) {

    if (exists('data_province') == FALSE) {
      stop("data_province is not present, run load_data_covid_dr()")
    }

    if (exists('data_cum') == FALSE) {
      stop("data_cum is not present, run load_data_covid_dr()")
    }

    if (exists('t3') == FALSE) {
      stop("Themes are not present, run load_themes()")
    }


    df_dths <-
      data_province  %>%
      filter(date == max(date)) %>%
      mutate(Let = 100 * Deaths / Cases,
             rank = rank(Deaths, ties.method = "first"),
             Reg = case_when(Province %in% Metropolitan ~ 'Metropolitan',
                             Province %in% North ~ 'North',
                             Province %in% South ~ 'South',
                             Province %in% East ~ 'East',
                             Province %in% NE ~ 'NOESP')) %>%
      drop_na()

    max_dths <-
      df_dths %>%
      summarise(max_dths = round(max(Deaths))) %>%
      select(max_dths) %>%
      as.integer() %>%
      round(digits = -1) %>%
      sum(25)

rep_actual <- data_cum$Reports %>% max(na.rm = TRUE)
    total_dths <- sum(df_dths$Deaths)
    total_let <- round(100 * total_dths/sum(df_dths$Cases),
                       digits = 2)

    lab_prov_dths <-
      labs(title = "DR: Deaths by COVID-19",
           subtitle  =  paste0("Total = ",
                               total_dths,
                               " (Let = ",
                               total_let,
                               "%)"),
           caption  = paste0("Source: @fidelmorla with information from special bulletin #",
                             rep_actual,
                             " of @SaludPublicaRD"),
           x = "",
           y = ""
      )

    prov_dths <-
      df_dths %>%
      filter(Deaths > 0) %>%
      summarise(TD = NROW(Deaths)) %>%
      as.integer()

    heatcol_dths <- sequential_hcl(n = prov_dths,
                                  palette = "YlOrRd",
                                  power = 0.5,
                                  l = 30,
                                  c = 225,
                                  c1 = 150
    )

    g_dths <-
      df_dths %>%
      filter(Deaths > 0) %>%
      ggplot(aes(x = reorder(Province, rank, order = TRUE),
                 y = Deaths,
                 fill = reorder(Province, -rank, order = TRUE),
                 col = reorder(Province, -rank, order = TRUE))) +
      geom_col() +
      scale_y_continuous(labels = scales::comma,
                         breaks = c(seq(0,
                                        max_dths,
                                        max_dths / 5)),
                         limits = c(0,
                                    max_dths)) +
      geom_text(aes(size = 18,
                    label = paste0(sprintf("%d",
                                           round(Deaths,
                                                 digits = 0)),
                                   " (",
                                   sprintf("%.1f%%",
                                           round(Let,
                                                 digits = 1)),
                                   ")")),
                hjust = -0.25) +
      scale_fill_manual(values = heatcol_dths) +
      scale_color_manual(values = heatcol_dths) +
      coord_flip() +
      lab_prov_dths +
      theme_clean() +
      t6 +
      theme(axis.text.x = element_text(angle = 0),
            axis.text.y = element_text(color = rev(heatcol_dths)))

    print(g_dths)

    if (saveplot == TRUE){
    assign('g_deaths', g_dths, envir = .GlobalEnv)
    }

    if (savepng == TRUE) {
      ggsave(filename = "deaths.png",
             plot = g_dths,
             device = "png",
             width = 18.333333333333332 / 1.5,
             height = 10.466666666666667 / 1.5,
             units = "in")
    }

  }

