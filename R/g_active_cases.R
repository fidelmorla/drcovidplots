#' @title Active cases of COVID19
#' @aliases g_active_cases
#' @description This function graphs the actives cases cases of COVID19 in the DR.
#' @usage g_active_cases(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Chart of COVID19 active cases and save a copy in png format on the computer
#' at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_active_cases(saveplot = FALSE, savepng = TRUE)
#' @name g_active_cases

g_active_cases <- function(saveplot = FALSE,
                       savepng = FALSE){

  if (exists('data_cum') == FALSE) {
    stop("data objects are missing, run load_data_covid_dr()")
  }

  df_ac <-
    data_cum  %>%
    mutate(date = ymd(date),
           ac = Positive - Deaths - Recovered) %>%
    select(date, ac)

  lab_ac <-
    labs(title = "DR: COVID19 active cases",
         subtitle = 'AC = Positive - Deaths - Recovered',
         caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
         x = "",
         y = ""
    )

  max_ac <-
    df_ac %>%
    summarise(max_ac = max(ac)) %>%
    as.integer() %>%
    round(digits = -3) %>%
    sum(1000)

  lim_dates <- c(ymd(
    data_cum %>%
      select(date) %>%
      filter(date == min(date)) %$%
      date),
    data_cum %>%
      select(date) %>%
      filter(date == max(date)) %$%
      date + 5)

  g_ac <-
    df_ac %>%
    ggplot(aes(x = date, y = ac)) +
    geom_area(fill = "royalblue") +
    scale_x_date(limits = lim_dates,
                 date_labels = "%d %b",
                 date_breaks = "3 days") +
    scale_y_continuous(labels = scales::comma,
                       breaks = c(seq(0, max_ac, max_ac / 4)),
                       limits = c(0,max_ac)) +
    geom_text(data = df_ac %>% tail(1),
              #check_overlap = TRUE,
              size = 4.5,
              vjust = -0.5,
              hjust = -0.15,
              show.legend = FALSE,
              aes(col = "l", label = scales::comma(ac))) +
    scale_fill_manual(values = c("royalblue", 'royalblue')) +
    scale_color_manual(values = c("royalblue", 'royalblue')) +
    lab_ac +
    drcovidplots::list_themes['t6']

  print(g_ac)

  if (saveplot == TRUE) {assign('g_ac', g_ac, envir = .GlobalEnv)}

  if (savepng == TRUE) {
    ggsave(filename = "active.png",
           plot = g_ac,
           device = "png",
           width = 18.333333333333332 / 1.5,
           height = 10.466666666666667 / 1.5,
           units = "in")
  }

}

