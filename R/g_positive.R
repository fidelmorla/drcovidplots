#' @title COVID19 positivity
#' @aliases g_positive
#' @description This function graphs the percentage of positives with respect to the total number of tests.
#' @usage g_positive
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.

#' @return Graph of the percentage of positives with respect to the total number of tests and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_positive()
#' g_positive(saveplot = FALSE, savepng = FALSE)
#' @name g_positive


g_positive <- function(saveplot = FALSE,
                       savepng = FALSE){

    if (exists('data_cum') == FALSE) {
      stop("data_cum is not present, run load_data_covid_dr()")
    }
    if (exists('t3') == FALSE) {
      stop("Themes are not present, run load_themes()")
    }


# Linea porcentaje de positivos -------------------------------------------

df_per_pos <-
  data_cum  %>%
  mutate(Pos_p = 100 * Positive / Tests) %>%
  select(date, Pos_p) %>%
  filter(date > max(date) - 15) %>%
  drop_na()

lab_per_pos <-
  labs(title = "DR: Positive cases / Tests of COVID-19",
       subtitle  =  paste0("%"),
       caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
       x = "",
       y = ""
  )

min_per_pos <-
  min(df_per_pos$Pos_p, na.rm = TRUE) %>%
  round(digits = -1)

max_per_pos <-
  max(df_per_pos$Pos_p, na.rm = TRUE) %>%
  round(digits = -1) %>%
  sum(5)

range_per_pos <- (max_per_pos - min_per_pos) / 2

g_per_pos <-
  df_per_pos %>%
  ggplot(aes(x = date, y = Pos_p)) +
  geom_text(show.legend = FALSE, aes(col = "blue",
                                     size = 24,
                                     label = sprintf("%.1f",round(Pos_p,1))),
            vjust = -0.5) +
  geom_line(aes(col = "blue")) +
  geom_point(aes(col = "blue"), size = 2) +
  scale_color_manual(values = c("white", "white")) +
  scale_fill_manual(values = c("white", "white")) +
  scale_y_continuous(breaks = c(seq(min_per_pos,max_per_pos, range_per_pos)),
                     limits = c(min_per_pos,max_per_pos)) +
  lab_per_pos +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "1 day") +
  t_darkred

print(g_per_pos)

if (saveplot == TRUE) {assign('g_per_pos', g_per_pos, envir = .GlobalEnv)}

if (savepng == TRUE){

ggsave(filename = "per_pos.png",
       plot = g_per_pos,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}

}
