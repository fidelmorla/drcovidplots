#' @title Growth rate of positive cases of COVID19
#' @aliases g_growth_cases
#' @description This function graphs the growth rate of positive cases of COVID19 in DR.
#' @usage g_growth_cases(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Graph of the growth rate of positive cases and save a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_growth_cases(saveplot = TRUE, savepng = FALSE)
#' @name g_growth_cases

g_growth_cases <- function(saveplot = FALSE,
                           savepng = FALSE){

    if (exists('data_province') == FALSE) {
      stop("data_province is not present, run load_data_covid_dr()")
    }

    if (exists('data_cum') == FALSE) {
      stop("data_cum is not present, run load_data_covid_dr()")
    }
    if (exists('t3') == FALSE) {
      stop("Themes are not present, run load_themes()")
    }



df_crec_pos <-
  data_cum  %>%
  mutate(CP = round(100 * (Positive / lag(Positive) - 1),2) ) %>%
  select(date, CP) %>%
  drop_na()

lab_crec_pos <-
  labs(title = "DR: Daily growth of COVID-19 positive cases",
       subtitle  =  "%",
       caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
       x = "",
       y = ""
  )

max_cp <-
  df_crec_pos %>%
  filter(date > max(date) - 12) %>%
  summarise(max_cp = max(CP, na.rm = TRUE)) %>%
  as.integer() %>%
  round(digits = -2) %>%
  sum(5)

g_dailygrowth <-
  df_crec_pos %>%
  filter(date > max(date) - 12) %>%
  ggplot(aes(x = date, y = CP)) +
  stat_smooth(method = "lm",
              formula= y ~ poly(x, 3),
              span = 0.8,
              level = 0.99,
              se = FALSE,
              linetype = 3,
              color = "red",
              size = 0.5,
              fill = "pink",
              fullrange = TRUE) +
  geom_text(show.legend = FALSE, aes(col = "blue",
                                     size = 24,
                                     label = sprintf("%.1f",round(CP,2))),
            vjust = -0.5) +
  geom_line(aes(col = "blue")) +
  geom_point(aes(fill = "blue"), size = 2) +
  scale_color_manual(values = "royalblue") +
  scale_fill_manual(values = "#0a448f") +
  scale_y_continuous(breaks = c(seq(0, max_cp, 2)),
                     limits = c(0,max_cp)) +
  lab_crec_pos +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "3 days") +
  t6

print(g_dailygrowth)

if (saveplot == TRUE){
assign('g_dailygrowth', g_dailygrowth, envir = .GlobalEnv)
}

if (savepng == TRUE) {

ggsave(filename = "dailygrowth.png",
       plot = g_dailygrowth,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}

}
