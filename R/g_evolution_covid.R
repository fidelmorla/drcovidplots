#' @title Evolution of new cases and total cases of COVID19
#' @aliases g_evolution_covid
#' @description This function generates a gif with the evolution of the new cases and total cases of COVID19 in DR.
#' @usage g_evolution_covid(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return GIF with the evolution of the new cases and total cases and save a
#' copy to the computer at the address defined in \code{setwd()}.
#' @importFrom scales comma
#' @export
#' @examples
#' g_evolution_covid()
#' g_evolution_covid(saveplot = TRUE)
#' @name g_evolution_covid

g_evolution_covid <- function(saveplot = FALSE,
                              savepng = FALSE){

    if (exists('data_province') == FALSE) {
      stop("data objects are missing, run load_data_covid_dr()")
    }

   df_cov <-
      data_cum %>%
      select(date, Positive, New_positive) %>%
      gather(key = "covid", value = "N", -date) %>%
      mutate(N = as.integer(N))

    line1 <- colorRampPalette(c("orange","red"))
    line2 <- colorRampPalette(c("steelblue","darkred"))

    max_cov <-
      df_cov %>%
      summarise(max_N = round(x = max(N),
                              digits = -2)) %>%
      select(max_N) %>%
      round(digits = -3) %>%
      as.integer()

    lim_dates <- c(ymd(
      data_cum %>%
        select(date) %>%
        filter(date == min(date)) %$%
        date ),
      data_cum %>%
        select(date) %>%
        filter(date == max(date)) %$%
        date + 5
    )


    lab_lines <-
      labs(title = "DR: COVID-19 evolution",
           caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
           x = "",
           y = ""
      )

    g_cov <-
      df_cov %>%
      ggplot(aes(x = date, y = N, col = covid)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("#bf085a", "#116396")) +
      geom_segment(aes(xend = date, yend = N),
                   linetype = 2) +
      facet_wrap(. ~ covid, ncol = 1, scales = "free_y")  +
      geom_point(size = 2) +
      geom_text(show.legend = FALSE, aes(size = 24,
                                         label = scales::comma(N, accuracy = 1)),
                hjust = -0.35) +
      lab_lines +
      scale_x_date(limits = lim_dates,
                   date_labels = "%d %b",
                   date_breaks = "3 days") +
      scale_y_continuous(labels = scales::comma) +
      transition_reveal(date)  +
      coord_cartesian(clip = 'off') +
      list_themes['t6']

    g_cov <- animate(g_cov,
            width = 1100/1.5,
            height = 628/1.5,
            end_pause = 35)

    print(g_cov)

    if (saveplot == TRUE) {
      assign('g_cov', g_cov, envir = .GlobalEnv)
      }

    if (savepng == TRUE) {

      anim_save(filename = "evolution_covid19.gif",
                animation = g_cov)
}
   }

