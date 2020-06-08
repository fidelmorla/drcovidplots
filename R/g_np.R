#' @title Negative and positive cases of COVID19
#' @aliases g_np
#' @description This function graphs the total number negative and positive COVID19 cases
#' in the Dominican Republic.
#' @usage g_np(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Graph of the total number of negative and positives cases and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_np()
#' g_np(saveplot = FALSE, savepng = TRUE)
#' @name g_np

g_np <- function(saveplot = FALSE,
                 savepng = TRUE){
    if (exists('data_cum') == FALSE) {
      stop("data_cum is not present, run load_data_covid_dr()")
    }

    if (exists('t3') == FALSE) {
      stop("Themes are not present, run load_themes()")
    }


df_np <-
  data_cum  %>%
  mutate(CP = round(100 * (Positive / lag(Positive) - 1),2),
         Pos_p = 100 * Positive / Tests) %>%
  select(date, Positive, Negative) %>%
  drop_na() %>%
  gather(key = "Cases", value = "N", -date) %>%
  mutate(
    label_N = ifelse(N == max(N[Cases == 'Positive']) | N == max(N[Cases == 'Negative']),
                     N,
                     NA))
total_tests <-
  df_np %>%
  summarise(total_tests = sum(label_N, na.rm = TRUE)) %>%
  as.integer()

max_total_tests <- round(total_tests, -3) %>%
  as.integer() %>%
  sum(1000)

per_pos <-
  data_cum  %>%
  mutate(CP = round(100 * (Positive / lag(Positive) - 1),2),
         Pos_p = 100 * Positive / Tests) %>%
  filter(date == max(date)) %>%
  select(Pos_p) %>%
  as.numeric() %>%
  round(digits = 2)

lab_np <-
  labs(title = "RD: Positive and negative tests of COVID-19",
       subtitle  =  paste0("Total tests = ",
                           comma(total_tests),
                           " (Positive = ",
                           per_pos,
                           "%)"),
       caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
       x = "",
       y = ""
  )


g_np <-
  df_np %>%
  ggplot(aes(x = date,
             y = N,
             color = Cases,
             fill = Cases,
             label = comma(label_N))) +
  geom_bar(position = "stack",
           stat="identity",
           width = 1,
           color = NA) +
  scale_fill_manual(values = c("#d1d1d1", '#850000')) +
  scale_color_manual(values = c("#850000", 'white')) +
  geom_text(angle = 90,
            check_overlap = TRUE,
            size = 3,
            show.legend = FALSE,
            position = position_stack(vjust = 0.5)) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "2 days") +
  scale_y_continuous(labels = scales::comma,
                     breaks = c(seq(0,max_total_tests,max_total_tests / 4)),
                     limits = c(0,max_total_tests)) +
  lab_np +
  t_legend

print(g_np)

if(saveplot == TRUE)  {assign('g_np', g_np, envir = .GlobalEnv)}

if (savepng == TRUE){
  ggsave(filename = "np.png",
         plot = g_np,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

}
