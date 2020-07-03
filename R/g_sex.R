#' @title Distribution by sex of COVID19 positives in the Dominican Republic
#' @aliases g_sex

#' @description This function graphs the distribution according to the sex of the positives.
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.
#' @param savepng Logical. Should save a png version of the plot? Default \code{FALSE}.

#' @usage g_sex(saveplot = FALSE, savepng = FALSE)
#' @return Graph of the distribution according to sex of the positives and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_sex()
#' g_sex(saveplot = FALSE, savepng = TRUE)
#' @name g_sex

g_sex <- function(saveplot = FALSE,
                  savepng = FALSE){
    if (exists('data_cum') == FALSE) {
      stop("data objects are missing, run load_data_covid_dr()")
    }

# Sexo --------------------------------------------------------------------

df_sex <-
  data_sex %>%
  mutate(Por = 100 * Positive / sum(Positive))

total_positive <-
  df_sex$Positive %>%
  sum() %>%
  as.integer()

max_sex <-
  df_sex$Positive %>%
  max() %>%
  as.integer() %>%
  round(digits = -3) + 1000


rep_actual <-
  data_cum$Reports %>% max(na.rm = TRUE)

lab_sex <-
  labs(title = "DR: Distribution by sex of COVID-19 positives cases",
       subtitle  =  paste0('Total positives cases = ',
                           comma(total_positive),
                           ' (% del total)'),
       caption  = paste0("Source: @fidelmorla with the special bulletin #",
                         rep_actual,
                         "a of @SaludPublicaRD"),

       x = "",
       y = ''
  )

col_sex <- c('#149ac7', #Hom
             '#b53c8d') #Muj

g_positives_by_sex <-
  df_sex %>%
  ggplot(aes(x = reorder(Sex, Positive, order = TRUE),
             y = Positive,
             fill = reorder(Sex, -Positive, order = TRUE),
             col = reorder(Sex, -Positive, order = TRUE))) +
  geom_col() +
  scale_y_continuous(labels = comma,
                     breaks = c(seq(0,max_sex,max_sex/4)),
                     limits = c(0,max_sex + 1000)) +
  geom_text(aes(size = 18,
                label = paste0(comma(round(Positive,
                                             digits = 0)),
                               " (",
                               sprintf("%.1f%%",
                                       round(Por,
                                             digits = 1)),
                               ")")),
            hjust = -0.25) +
  scale_fill_manual(values = col_sex) +
  scale_color_manual(values = col_sex) +
  coord_flip() +
  lab_sex +
  theme_clean() +
  drcovidplots::list_themes['t6'] +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(color = rev(col_sex)))

print(g_positives_by_sex) %>% suppressWarnings()

if (saveplot == TRUE) {assign('g_positives_by_sex', g_positives_by_sex, envir = .GlobalEnv)}

if (savepng == TRUE){

  ggsave(filename = "positive_by_sex.png",
         plot = g_positives_by_sex,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

}
