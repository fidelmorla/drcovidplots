#' @title COVID19 tests in the Dominican Republic
#' @aliases g_tests
#' @description This function graphs the COVID19 tests in RD.
#' @usage g_tests()
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @importFrom scales comma
#' @return Graph of daily and accumulated tests both in units and per million inhabitants
#' and save a copy in png format on the computer to the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_tests()
#' @name g_tests


g_tests <-
  function(savepng = FALSE){

    if (exists('data_province') == FALSE) {
      stop("data_province is not present, run load_data_covid_dr()")
    }

    if (exists('data_cum') == FALSE) {
      stop("data_cum is not present, run load_data_covid_dr()")
    }

    if (exists('t3') == FALSE) {
      stop("Themes are not present, run load_themes()")
    }


    Pop <- 10448499L

    df_tests <-
      tibble(date = ymd("2020-03-18"), Tests = 88) %>%
      rbind(data_cum  %>%
              mutate(date = ymd(date),
                     Tests = c(Tests - lag(Tests, n = 1L))
              ) %>%
              select(date, Tests) %>%
              drop_na()
      ) %>%
      mutate(Tests_Cum = cumsum(Tests),
             Tests_m = as.integer(Tests * 1000000 / Pop),
             Tests_Cum_m = cumsum(Tests_m),
      )


    lab_tests_m <-
      labs(title = "DR: Daily COVID-19 tests",
           subtitle  =  paste0('Total per million inhabitants'),
           caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
           x = "",
           y = ""
      )

    lab_tests_cum_m <-
      labs(title = "DR: Acumulated COVID-19 tests",
           subtitle  =  paste0('Total per million inhabitants'),
           caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
           x = "",
           y = ""
      )

    lab_tests <-
      labs(title = "DR: Daily COVID-19 tests",
           caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
           x = "",
           y = ""
      )

    lab_tests_cum <-
      labs(title = "DR: Acumulated COVID-19 tests",
           caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
           x = "",
           y = ""
      )


    max_tests_cum_m <-
      df_tests %>%
      summarise(max_tests = max(Tests_Cum_m)) %>%
      round(digits = -2) %>%
      as.integer()


    max_tests_m <-
      df_tests %>%
      summarise(max_tests = max(Tests_m)) %>%
      round(digits = -2) %>%
      as.integer() + 40

    max_tests_cum <-
      df_tests %>%
      summarise(max_tests = max(Tests_Cum)) %>%
      round(digits = -2) %>%
      as.integer() %>%
      sum(500)


    max_tests <-
      df_tests %>%
      summarise(max_tests = max(Tests)) %>%
      round(digits = -2) %>%
      as.integer() + 100

    g_tests <-
      df_tests %>%
      ggplot(aes(x = date, y = Tests)) +
      geom_pointline(aes(fill = "blue", col = "blue"), size = 2) +
      scale_x_date(date_labels = "%d %b",
                   date_breaks = "2 days") +
      scale_y_continuous(breaks = c(seq(0,
                                        max_tests,
                                        max_tests / 4)),
                         limits = c(0,max_tests),
                         labels = comma) +
      geom_text(data = tail(df_tests, 1),
                check_overlap = TRUE,
                size = 4.5,
                vjust = -1,
                hjust = 0.15,
                show.legend = FALSE,
                aes(col = "l", label = Tests)) +
      scale_fill_manual(values = c("white", 'white')) +
      scale_color_manual(values = c("white", 'white')) +
      lab_tests +
      t_darkblue

    g_tests_m <-
      df_tests %>%
      ggplot(aes(x = date, y = Tests_m)) +
      geom_pointline(aes(fill = "blue", col = "blue"), size = 2) +
      scale_x_date(date_labels = "%d %b",
                   date_breaks = "2 days") +
      scale_y_continuous(breaks = c(seq(0,
                                        max_tests_m,
                                        max_tests_m / 4)),
                         limits = c(0,max_tests_m),
                         labels = comma) +
      geom_text(data = tail(df_tests, 1),
                check_overlap = TRUE,
                size = 4.5,
                vjust = -0.5,
                hjust = -0.2,
                show.legend = FALSE,
                aes(col = "l", label = Tests_m)) +
      scale_fill_manual(values = c("white", 'white')) +
      scale_color_manual(values = c("white", 'white')) +
      lab_tests_m +
      t_darkblue

    g_tests_cum <-
      df_tests %>%
      ggplot(aes(x = date, y = Tests_Cum)) +
      geom_step(aes(col = "blue")) +
      scale_x_date(date_labels = "%d %b",
                   date_breaks = "2 days") +
      scale_y_continuous(breaks = c(seq(0, max_tests_cum + 500, (max_tests_cum + 500)/ 4)),
                         limits = c(0,max_tests_cum + 100),
                         labels = comma) +
      geom_text(data = tail(df_tests, 1),
                check_overlap = TRUE,
                size = 4.5,
                vjust = -0.5,
                hjust = 0.5,
                show.legend = FALSE,
                aes(col = "l", label = Tests_Cum)) +
      scale_fill_manual(values = c("white", 'white')) +
      scale_color_manual(values = c("white", 'white')) +
      lab_tests_cum +
      t_darkblue

    g_tests_cum_m <-
      df_tests %>%
      ggplot(aes(x = date, y = Tests_Cum_m)) +
      geom_step(aes(col = "blue")) +
      scale_x_date(date_labels = "%d %b",
                   date_breaks = "2 days") +
      scale_y_continuous(breaks = c(seq(0, max_tests_cum_m + 100, (max_tests_cum_m + 100)/ 4)),
                         limits = c(0,max_tests_cum_m + 100),
                         labels = comma) +
      geom_text(data = tail(df_tests, 1),
                check_overlap = TRUE,
                size = 4.5,
                vjust = -0.5,
                hjust = 0.5,
                show.legend = FALSE,
                aes(col = "l", label = Tests_Cum_m)) +
      scale_fill_manual(values = c("white", 'white')) +
      scale_color_manual(values = c("white", 'white')) +
      lab_tests_cum_m +
      t_darkblue

    assign('g_tests', g_tests, envir = .GlobalEnv)

    assign('g_tests_m', g_tests_m, envir = .GlobalEnv)

    assign('g_tests_cum', g_tests_cum, envir = .GlobalEnv)

    assign('g_tests_cum_m', g_tests_cum_m, envir = .GlobalEnv)

    if (savepng == TRUE){

    ggsave(filename = "tests.png",
           plot = g_tests,
           device = "png",
           width = 18.333333333333332 / 1.5,
           height = 10.466666666666667 / 1.5,
           units = "in")

    ggsave(filename = "tests_m.png",
           plot = g_tests_m,
           device = "png",
           width = 18.333333333333332 / 1.5,
           height = 10.466666666666667 / 1.5,
           units = "in")

    ggsave(filename = "tests_cum.png",
           plot = g_tests_cum,
           device = "png",
           width = 18.333333333333332 / 1.5,
           height = 10.466666666666667 / 1.5,
           units = "in")

    ggsave(filename = "tests_cum_m.png",
           plot = g_tests_cum_m,
           device = "png",
           width = 18.333333333333332 / 1.5,
           height = 10.466666666666667 / 1.5,
           units = "in")
}
  }
