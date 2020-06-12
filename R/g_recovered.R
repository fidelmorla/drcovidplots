#' @title Recovered from COVID19
#' @aliases g_recovered
#' @description This function graphs those recovered from COVID19 in the Dominican Republic.
#' @usage g_recovered(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Graph of the recovered persons and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @importFrom scales comma
#' @export
#' @examples
#' g_recovered()
#' g_recovered(saveplot = TRUE, savepng = FALSE)
#' @name g_recovered

g_recovered <- function(saveplot = FALSE,
                        savepng = FALSE){

  if (exists('data_province') == FALSE) {
    stop("data objects are missing, run load_data_covid_dr()")
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
  round(digits = -3) %>%
  sum(1000)

g_rec <-
  df_rec %>%
  ggplot(aes(x = date, y = Recovered)) +
  geom_pointline(aes(fill = "blue", col = "blue"), size = 2) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "3 days") +
  scale_y_continuous(labels = comma,
                     breaks = c(seq(0,max_rec, max_rec / 4)),
                     limits = c(0,max_rec)) +
  geom_text(data = tail(df_rec, n = 1),
            check_overlap = TRUE,
            size = 4.5,
            vjust = -0.55,
            hjust = 0.55,
            show.legend = FALSE,
            aes(col = "l", label = comma(Recovered))) +
  scale_fill_manual(values = c("white", 'white')) +
  scale_color_manual(values = c("white", 'white')) +
  lab_rec +
  list_themes['t_darkgreen']

print(g_rec)

if (saveplot == TRUE) {assign('g_rec', g_rec, envir = .GlobalEnv)}


if (savepng == TRUE){

  ggsave(filename = "recovered.png",
         plot = g_rec,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

}
