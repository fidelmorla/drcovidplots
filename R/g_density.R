#' @title Correlation between COVID19 cases and population density
#' @aliases g_density
#' @description This function graphs the correlation between cases of COVID19
#' and population density of COVID19 in DR.
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @usage g_density(saveplot = FALSE, savepng = FALSE)
#' @return Graph of daily and accumulated tests both in units and per million inhabitants
#' and save a copy in png format on the computer to the address defined in \code{setwd()}.
#' @importFrom scales comma
#' @export
#' @examples
#' g_density(saveplot = FALSE, savepng = TRUE)
#' @name g_density

g_density <- function(saveplot = FALSE,
                      savepng = FALSE){

  if (exists('data_province') == FALSE) {
    stop("data objects are missing, run load_data_covid_dr()")
  }


df_den_cases <-
data_province %>%
  filter(date == max(date),
         Province != 'NOESP') %>%
  select(Province, Cases) %>%
  left_join(
    y = data_density %>%
      select(Province, Density),
    by = "Province") %>%
  mutate(Density = round(Density, digits = 3))

heatcol_den <- sequential_hcl(n = 32,
                              palette = "YlOrRd",
                              power = 0.5,
                              l = 30,
                              c = 225,
                              c1 = 150)

max_den <- 14

cor_den <-
  cor(x = df_den_cases %>% select(Cases, Density)) %>%
  round(digits = 4)

lab_den <-
  labs(title = "DR: Correlation between Population Density and Positive Cases of COVID-19",
       subtitle  =  paste0('Correlation coefficient = ',
                           cor_den[2]),
       caption  = paste0("Source: @fidelmorla with the special bulletin #",
                         rep_actual,
                         "of @SaludPublicaRD and reports of surface and population of @ONERD_"),
       x = "",
       y = expression ("Density = Population / Area in"~m^2)
  )

g_den <-
  df_den_cases %>%
  ggplot(aes(x = reorder(Province, Density),
             y = Density,
             color = reorder(Province, -Density),
             fill = reorder(Province, -Density),
             size = Density)) +
  geom_point() +
  geom_text(aes(label = Cases),
            size = 4,
            hjust = -1) +
  geom_text(aes(label = Density),
            size = 4,
            hjust = 1.5) +
  annotate_textp(x = 1,
                 y = .5,
                 size = 12,
                 color = "darkred",
                 label = "Density (left) . Cases (right)") +
  scale_color_manual(values = heatcol_den) +
  scale_fill_manual(values = heatcol_den) +
  scale_y_continuous(labels = scales::comma,
                     breaks = c(seq(0,max_den, max_den/ 2)),
                     limits = c(-1,max_den + 1)) +
  coord_flip() +
  lab_den +
  list_themes['t6'] +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(color = rev(heatcol_den)))

print(g_den)

if (saveplot == TRUE) {
  assign('g_den', g_den, envir = .GlobalEnv)
}

if (savepng == TRUE) {

ggsave(filename = "density.png",
       plot = g_den,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}

}
