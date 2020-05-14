#' @title People in hospital isolation for COVID19
#' @aliases g_hospital
#' @description This function graphs the people in hospital isolation of the positive cases of COVID19 in the DR.
#' @usage g_hospital()
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Chart of isolated individuals in hospitals and save a copy in png format on the computer
#' at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_hospital()
#' @name g_hospital

g_hospital <- function(savepng = FALSE){

    if (exists('data_cum') == FALSE) {
      stop("data_cum is not present, run load_data_covid_dr()")
    }
  if (exists('t3') == FALSE) {
    stop("Themes are not present, run load_themes()")
  }


df_h <-
  data_cum  %>%
  mutate(date = ymd(date)) %>%
  select(date, H) %>%
  filter(H > 0)

lab_h <-
  labs(title = "DR: People in hospital isolation by COVID-19",
       caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
       x = "",
       y = ""
  )

max_h <-
  df_h %>%
  summarise(max_rec = max(H)) %>%
  as.integer() %>%
  round(digits = -2) %>%
  sum(100)

g_h <-
  df_h %>%
  ggplot(aes(x = date, y = H)) +
  geom_step(aes(col = "blue")) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "2 days") +
  scale_y_continuous(breaks = c(seq(0,max_h, max_h / 4)),
                     limits = c(0,max_h)) +
  geom_text(data = df_h %>% tail(1),
            check_overlap = TRUE,
            size = 4.5,
            vjust = -0.5,
            show.legend = FALSE,
            aes(col = "l", label = H)) +
  scale_fill_manual(values = c("white", 'white')) +
  scale_color_manual(values = c("white", 'white')) +
  lab_h +
  t_darkorange

assign('g_h', g_h, envir = .GlobalEnv)
if (savepng == TRUE) {
  ggsave(filename = "hospital.png",
       plot = g_h,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}
return(print(.GlobalEnv$g_h))
}

