#' @title Recovered by province from COVID19 in the Dominican Republic
#' @aliases g_recovered_province
#' @description This function graphs those recovered from COVID19 in the Dominican Republic ordered by provinces.
#' @usage g_recovered_province(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.

#' @return Graph of the recovered persons ordered by provinces and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_recovered_province(saveplot = FALSE, savepng = TRUE)
#' @name g_recovered_province


g_recovered_province <- function(saveplot = FALSE,
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



# Tasa de recuperaci?n ----------------------------------------------------

df_rec_prov <-
  data_province  %>%
  filter(date == max(date)) %>%
  mutate(TR = 100 * Recovered / Cases,
         rank = rank(Recovered, ties.method = "first")) %>%
  drop_na()


rep_actual <-
  data_cum$Reports %>% max(na.rm = TRUE)


max_rec_p <-
  df_rec_prov %>%
  summarise(max_rec_prov = round(max(Recovered))) %>%
  select(max_rec_prov) %>%
  as.integer() %>%
  round(digits = -2) %>%
  sum(500)

total_rec <- sum(df_rec_prov$Recovered)
total_tr <- round(100 * total_rec/sum(df_rec_prov$Cases),
                  digits = 2)


lab_rec_prov <-
  labs(title = "DR: Recovered de COVID-19 por province",
       subtitle  =  paste0("Total = ",
                           comma(total_rec),
                           " (Recovery rate = ",
                           total_tr,
                           "%)"),
       caption  = paste0("Source: @fidelmorla with information from special bulletin #",
                         rep_actual,
                         " of @SaludPublicaRD"),
       x = "",
       y = ""
  )

#Color
prov_rec <-
  df_rec_prov %>%
  filter(Recovered > 0) %>%
  summarise(TR = NROW(Recovered)) %>%
  as.integer()

heatcol_rec <- sequential_hcl(n = prov_rec,
                              palette = "PuBuGn",
                              power = 0.5,
                              l = 30,
                              c = 225,
                              c1 = 150)

g_rec_prov <-
  df_rec_prov %>%
  filter(Recovered > 0) %>%
  ggplot(aes(x = reorder(Province, rank, order = TRUE),
             y = Recovered,
             fill = reorder(Province, -rank, order = TRUE),
             col = reorder(Province, -rank, order = TRUE))) +
  geom_col() +
  scale_y_continuous(breaks = c(seq(0,
                                    max_rec_p,
                                    max_rec_p / 4)),
                     limits = c(0,
                                max_rec_p),
                     labels = comma) +
  geom_text(aes(size = 16,
                label = paste0(comma(round(Recovered,
                                             digits = 0)),
                               " (",
                               sprintf("%.1f%%",
                                       round(TR,
                                             digits = 1)),
                               ")")),
            hjust = -0.25) +
  scale_fill_manual(values = heatcol_rec) +
  scale_color_manual(values = heatcol_rec) +
  coord_flip() +
  lab_rec_prov +
  theme_clean() +
  t6 +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(color = rev(heatcol_rec)))

print(g_rec_prov)

if (saveplot == TRUE){
assign('g_rec_prov', g_rec_prov, envir = .GlobalEnv)
}

if (savepng == TRUE){

  ggsave(filename = "recovered_province.png",
         plot = g_rec_prov,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

}
