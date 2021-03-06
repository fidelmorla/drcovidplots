#' @title People in hospital isolation for COVID19
#' @aliases g_hospital
#' @description This function graphs the people in hospital isolation of the positive cases of COVID19 in the DR.
#' @usage g_hospital(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.
#' @param savepng Logical. Should save a png version of the plot? Default \code{FALSE}.
#' @return Chart of isolated individuals in hospitals and save a copy in png format on the computer
#' at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_hospital(saveplot = FALSE, savepng = TRUE)
#' @name g_hospital

g_hospital <- function(saveplot = FALSE,
                       savepng = FALSE){

    if (exists('data_cum') == FALSE) {
      stop("data objects are missing, run load_data_covid_dr()")
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

lim_dates <- c(ymd(
  data_cum %>%
    select(date) %>%
    filter(date == min(date)) %$%
    date ),
  data_cum %>%
    select(date) %>%
    filter(date == max(date)) %$%
    date + 5
)

g_h <-
  df_h %>%
  ggplot(aes(x = date, y = H)) +
  geom_step(aes(col = "blue")) +
  scale_x_date(limits = lim_dates,
               date_labels = "%d %b",
               date_breaks = "3 days") +
  scale_y_continuous(labels = scales::comma,
                     breaks = c(seq(0,max_h, max_h / 4)),
                     limits = c(0,max_h)) +
  geom_text(data = df_h %>% tail(1),
            #check_overlap = TRUE,
            size = 4.5,
            vjust = -0.5,
            hjust = -0.15,

            show.legend = FALSE,
            aes(col = "l", label = H)) +
  scale_fill_manual(values = c("white", 'white')) +
  scale_color_manual(values = c("white", 'white')) +
  lab_h +
  drcovidplots::list_themes['t_darkorange']

print(g_h) %>% suppressWarnings()

if (saveplot == TRUE) {assign('g_h', g_h, envir = .GlobalEnv)}

if (savepng == TRUE) {
  ggsave(filename = "hospital.png",
       plot = g_h,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}

}

