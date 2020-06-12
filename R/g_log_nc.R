#' @title New cases vs total cases in logarithms of COVID19
#' @aliases g_log_nc
#' @description This function graphs the logarithm of both new cases and total cases
#' in the Dominican Republic
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @usage g_log_nc(saveplot = FALSE, savepng = FALSE)
#' @return Graph of the logarithm of of both new cases and total cases and save a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_log_nc()
#' g_log_nc(saveplot = FALSE, savepng = TRUE)
#' @name g_log_nc

g_log_nc <- function(saveplot = FALSE,
                     savepng = FALSE){
    if (exists('data_cum') == FALSE) {
      stop("data objects are missing, run load_data_covid_dr()")
    }

    df_nc <-
      data_cum  %>%
      mutate(Let = 100 * Deaths / Positive,
             lnc = c(NA, NA, NA, log(rollmeanr(New_positive,
                                               k = 4))),
             lpos = c(NA, NA, NA, log(rollmeanr(Positive,
                                                k = 4)))) %>%
      drop_na()

    seqmax_nc <- NROW(df_nc$New_positive)

    lab_nc <-
      labs(title = "DR: Total positive cases vs. New cases of COVID-19",
           subtitle  =  paste0("In logarithms (4-day moving average)"),
           caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
           x = "Log(Positive)",
           y = "Log(New positives)"
      )

    g_nc <-
      df_nc %>%
      ggplot(aes(x = lpos, y = lnc)) +
      geom_line(aes(col = "blue")) +
      geom_point(aes(fill = "blue"), size = 1) +
      scale_color_manual(values = "royalblue") +
      scale_fill_manual(values = "darkblue") +
      scale_x_continuous(breaks = c(seq(3,max(df_nc$lpos) + 0.5,0.5)),
                         limits = c(3,max(df_nc$lpos) + 0.5)) +
      scale_y_continuous(breaks = c(seq(2,round_half_up(max(df_nc$lnc)) + 0.5,0.5)),
                         limits = c(2,round_half_up(max(df_nc$lnc)) + 0.5)) +
      lab_nc +
      annotate("text",
               y = max(df_nc$lnc) * 0.95,
               x = min(df_nc$lpos) * 1.10,
               colour = "darkred",
               label = "We are winning \n when this number GOES DOWN!") +
      drcovidplots::list_themes['t6']

    print(g_nc)

    if (saveplot == TRUE) {assign('g_nc', g_nc, envir = .GlobalEnv)}

    if (savepng == TRUE){
      ggsave(filename = "log_cases.png",
             plot = g_nc,
             device = "png",
             width = 18.333333333333332 / 1.5,
             height = 10.466666666666667 / 1.5,
             units = "in")
    }

}
