#' @title Daily negative and positive cases of COVID19.
#' @aliases g_np_daily
#' @description This function graphs the daily total number negative and positive COVID19 cases
#' in the Dominican Republic.
#' @usage g_np_daily()
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.

#' @return Graph of the daily total number of negative and positives cases and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_np_daily()
#' @name g_np_daily


g_np_daily <-
  function(savepng = FALSE){
    if (exists('data_province') == FALSE) {
      stop("data_province is not present, run load_data_covid_dr()")
    }

    if (exists('data_cum') == FALSE) {
      stop("data_cum is not present, run load_data_covid_dr()")
    }
    if (exists('t3') == FALSE) {
      stop("Themes are not present, run load_themes()")
    }


df_np_daily <-
  data_cum  %>%
  mutate(ND = Negative - dplyr::lag(Negative)) %>%
  select(date, New_positive, ND) %>%
  rename(Negative = ND,
         Positive = New_positive) %>%
  drop_na() %>%
  gather(key = "Cases", value = "N", -date)

mean_pos <-
  df_np_daily %>%
  summarise(mean_pos = mean(N[Cases == 'Positive'],
                            na.rm = TRUE)) %>%
  as.integer()

mean_neg <-
  df_np_daily %>%
  summarise(mean_pos = mean(N[Cases == 'Negative'],
                            na.rm = TRUE)) %>%
  as.integer()

max_daily_total_tests <-
  df_np_daily %>%
  summarise(
    max = sum(max(N[Cases == 'Negative'],
                  na.rm = TRUE),
              max(N[Cases == 'Positive'],
                  na.rm = TRUE))) %>%
  round(digits = -2) %>%
  sum(100) %>%
  as.integer()


lab_np_daily <-
  labs(title = "RD: Positive and negative cases COVID-19",
       subtitle  =  paste0("Daily average of negative test = ",
                           comma(mean_neg),
                           " / Daily average of positive test = ",
                           comma(mean_pos),
                           " "),
       caption  = "Source: @fidelmorla with special bulletins of @SaludPublicaRD",
       x = "",
       y = ""
  )


g_np_daily <-
  df_np_daily %>%
  ggplot(aes(x = date,
             y = N,
             color = Cases,
             fill = Cases,
             label = comma(N))) +
  geom_bar(position = "stack",
           stat="identity",
           width = 1,
           color = NA) +
  scale_fill_manual(values = c("#d1d1d1", '#850000')) +
  scale_color_manual(values = c("#850000", 'white')) +
  geom_text(data = df_np_daily %>% filter(date == max(date)),
            angle = 90,
            check_overlap = TRUE,
            size = 3,
            show.legend = FALSE,
            position = position_stack(vjust = 0.5)) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "2 days") +
  scale_y_continuous(breaks = c(seq(0,
                                    max_daily_total_tests,
                                    max_daily_total_tests / 4)),
                     limits = c(0,
                                max_daily_total_tests)) +
  lab_np_daily +
  t_legend

assign('g_np_daily', g_np_daily, envir = .GlobalEnv)
if (savepng == TRUE){

ggsave(filename = "dailynp.png",
       plot = g_np_daily,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}
return(print(.GlobalEnv$g_np_daily))

}
