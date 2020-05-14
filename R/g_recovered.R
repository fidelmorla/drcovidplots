#' @title Recovered from COVID19
#' @aliases g_recovered
#' @description This function graphs those recovered from COVID19 in the Dominican Republic.
#' @usage g_recovered()
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.

#' @return Graph of the recovered persons and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_recovered()
#' @name g_recovered

g_recovered <- function(savepng = FALSE){

  if (exists('data_province') == FALSE) {
    stop("data_province is not present, run load_data_covid_dr()")
  }

  if (exists('data_cum') == FALSE) {
    stop("data_cum is not present, run load_data_covid_dr()")
  }
  if (exists('t3') == FALSE) {
    stop("Themes are not present, run load_themes()")
  }


# Recovered -------------------------------------------------------------

df_rec <-
  data_cum  %>%
  mutate(date = ymd(date)) %>%
  select(date, Recovered)

lab_rec <-
  labs(title = "DR: Recovered from COVID-19",
       caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
       x = "",
       y = ""
  )

max_rec <-
  df_rec %>%
  summarise(max_rec = max(Recovered)) %>%
  as.integer() %>%
  round(digits = -2) %>%
  sum(100)

g_rec <-
  df_rec %>%
  ggplot(aes(x = date, y = Recovered, size = Recovered / 4)) +
  geom_pointline(aes(fill = "blue", col = "blue")) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "2 days") +
  scale_y_continuous(breaks = c(seq(0,max_rec, max_rec / 4)),
                     limits = c(0,max_rec)) +
  geom_text(check_overlap = TRUE,
            size = 4.5,
            vjust = -0.25,
            hjust = 1.5,
            show.legend = FALSE,
            aes(col = "l", label = ifelse(Recovered > 1300,
                                          Recovered,
                                          NA))) +
  scale_fill_manual(values = c("white", 'white')) +
  scale_color_manual(values = c("white", 'white')) +
  lab_rec +
  t_darkgreen

assign('g_rec', g_rec, envir = .GlobalEnv)


if (savepng == TRUE){

  ggsave(filename = "recovered.png",
         plot = g_rec,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}
return(print(.GlobalEnv$g_rec))
}
