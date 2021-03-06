#' @title Recovered growth rate from COVID19
#' @aliases g_recovered_growth
#' @description This function graphs the growth rate of those recovered from COVID19
#' in the Dominican Republic.
#' @usage g_recovered_growth(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.
#' @param savepng Logical. Should save a png version of the plot? Default \code{FALSE}.
#' @return Graph of the recovered persons growth rate and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_recovered_growth(saveplot = FALSE, savepng = TRUE)
#' @name g_recovered_growth


g_recovered_growth <- function(saveplot = FALSE,
                               savepng = FALSE){

if (exists('data_province') == FALSE) {
  stop("data objects are missing, run load_data_covid_dr()")
}

# Recovered -------------------------------------------------------------

df_rec_growth <-
  data_cum  %>%
  mutate(date = ymd(date)) %>%
  select(date, Recovered) %>%
  mutate(Rec_growth = 100 * (Recovered / lag(Recovered) - 1)) %>%
  filter(date >= max(date) - 15)

lab_rec_growth <-
  labs(title = "DR: Recovered from COVID19 growth rate",
       caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
       x = "",
       y = ""
  )

max_rec_growth <-
  df_rec_growth %>%
  summarise(max_rec_growth = max(Rec_growth)) %>%
  as.integer() %>%
  round(digits = -1) %>%
  sum(5)

g_rec_growth <-
  df_rec_growth %>%
  ggplot(aes(x = date, y = Rec_growth)) +
  geom_pointline(aes(fill = "blue", col = "blue")) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "3 days") +
  scale_y_continuous(breaks = c(seq(0,max_rec_growth, max_rec_growth / 5)),
                     limits = c(0,max_rec_growth)) +
  geom_text(data = tail(x = df_rec_growth, n = 1),
            check_overlap = TRUE,
            size = 4.5,
            vjust = 0,
            hjust = 0,
            show.legend = FALSE,
            aes(col = "blue", label = round(Rec_growth, digits = 2))) +
  scale_fill_manual(values = c("white", 'white')) +
  scale_color_manual(values = c("white", 'white')) +
  lab_rec_growth +
  drcovidplots::list_themes['t_darkgreen']

print(g_rec_growth) %>% suppressWarnings()

if (saveplot == TRUE) {assign('g_rec_growth', g_rec_growth, envir = .GlobalEnv)}

if (savepng == TRUE){

  ggsave(filename = "recovered_growth.png",
         plot = g_rec_growth,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

}
