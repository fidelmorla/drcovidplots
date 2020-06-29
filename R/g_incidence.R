#' @title Incidence of COVID19
#' @aliases g_incidence
#' @description This function graphs the incidence of COVID19 in DR.
#' @usage g_incidence(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.
#' @param savepng Logical. Should save a png version of the plot? Default \code{FALSE}.
#' @importFrom scales comma
#' @return Incidence chart and save a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_incidence()
#' g_incidence(saveplot = TRUE, savepng = FALSE)
#' @name g_incidence


g_incidence <- function(saveplot = FALSE,
                        savepng = FALSE){

      if (exists('data_province') == FALSE) {
        stop("data objects are missing, run load_data_covid_dr()")
      }


df_inc <-
  data_province %>%
  filter(date == max(date)) %>%
  left_join(
    y = data_density %>%
      select(Province, Pop),
    by = "Province")

df_inc %<>%
  mutate(Let = 100 * Deaths / Cases,
         Incidence = Cases * 100000 / Pop,
         Reg = case_when(Province %in% Metropolitan ~ 'Metropolitan',
                         Province %in% North ~ 'North',
                         Province %in% South ~ 'South',
                         Province %in% East ~ 'East',
                         Province %in% NE ~ 'NOESP')) %>%
  filter(Cases > 0) %>%
  drop_na(Incidence)


heatcol_inc <- sequential_hcl(n = 33,
                              palette = "YlOrRd",
                              power = 0.5,
                              l = 30,
                              c = 225,
                              c1 = 150)

heatcol_inc_reg <-
  c('#0b559e', #azul
    '#b50012', #rojo
    '#03996f', #verde
    '#805511') #marron


max_inc <-  df_inc$Incidence %>% max(na.rm = TRUE) %>%
  round(digits = -1) %>%
  sum(50)

rep_actual <- data_cum$Reports %>% max()

lab_inc <-
  labs(title = "DR: COVID19 incidence",
       subtitle  =  paste0('Positive cases per 100,000 inhabitants'),
       caption  = paste0("Source: @fidelmorla with information from special bulletin #",
                         rep_actual,
                         " of @SaludPublicaRD y population reports of @ONERD_"),
       x = "",
       y = ''
  )

g_inc <-
  df_inc %>%
  ggplot(aes(x = reorder(Province, Incidence),
             y = Incidence,
             color = reorder(Province, -Incidence),
             fill = reorder(Province, -Incidence))) +
  geom_col() +
  geom_text(show.legend = FALSE,
            aes(size = 24,
                label = sprintf("%.1f",round(Incidence,1))),
            hjust = -0.5) +
  annotate_textp(x = 1,
                 y = .5,
                 size = 12,
                 color = "darkred",
                 label = "100,000 * Positive / Population") +
  scale_color_manual(values = heatcol_inc) +
  scale_fill_manual(values = heatcol_inc) +
  scale_y_continuous(labels = scales::comma,
                     breaks = c(seq(0,max_inc, max_inc/ 4)),
                     limits = c(-5,max_inc)) +
  coord_flip() +
  lab_inc +
  drcovidplots::list_themes['t6'] +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(color = rev(heatcol_inc)))

print(g_inc)

if (saveplot == TRUE) {assign('g_inc', g_inc, envir = .GlobalEnv)}

if (savepng == TRUE) {

ggsave(filename = "incidence.png",
       plot = g_inc,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}

}
