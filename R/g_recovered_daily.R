#' @title Daily number of persons recovered from COVID19
#' @aliases g_recovered_daily
#' @description This function graphs the daily number of persons recovered from COVID19
#' in the Dominican Republic.
#' @usage g_recovered_daily()
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.

#' @return Graph of the daily recovered persons and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_recovered_daily()
#' @name g_recovered_daily

g_recovered_daily <- function(savepng = FALSE){

  if (exists('data_province') == FALSE) {
    stop("data_province is not present, run load_data_covid_dr()")
  }

  if (exists('data_cum') == FALSE) {
    stop("data_cum is not present, run load_data_covid_dr()")
  }
  if (exists('t3') == FALSE) {
    stop("Themes are not present, run load_themes()")
  }


df_rec_d <-
  data_cum  %>%
  mutate(date = ymd(date)) %>%
  select(date, N_recovered) %>%
  filter(date >= '2020-04-05')

lab_rec_d <-
  labs(title = "DR: Daily recovered persons from COVID-19",
       #subtitle  =  paste0('Individuals'),
       caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
       x = "",
       y = ""
  )

max_rec_d <-
  df_rec_d %>%
  summarise(max_rec_d = max(N_recovered)) %>%
  as.integer() %>%
  round(digits = -2) %>%
  sum(50)

g_rec_d <-
  df_rec_d %>%
  ggplot(aes(x = date, y = N_recovered)) +
  geom_pointline(aes(fill = "blue", col = "blue"), size = 2) +
  scale_x_date(labels = comma,
               date_labels = "%d %b",
               date_breaks = "3 days") +
  scale_y_continuous(breaks = c(seq(0,max_rec_d, max_rec_d / 5)),
                     limits = c(0,max_rec_d)) +
  geom_text(data = tail(df_rec_d, n = 1),
            check_overlap = TRUE,
            size = 4.5,
            vjust = -0.25,
            hjust = -0.1,
            show.legend = FALSE,
            aes(col = "l", label = N_recovered)) +
  scale_fill_manual(values = c("white", 'white')) +
  scale_color_manual(values = c("white", 'white')) +
  lab_rec_d +
  t_darkgreen

assign('g_rec_d', g_rec_d, envir = .GlobalEnv)


if (savepng == TRUE){

  ggsave(filename = "recovered_daily.png",
         plot = g_rec_d,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

return(print(.GlobalEnv$g_rec_d))

}
