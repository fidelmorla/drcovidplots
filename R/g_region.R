#' @title COVID19 cases by region
#' @aliases g_region
#' @description This function graphs the positive cases of COVID19 by region in the DR.
#' @usage g_region(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.
#' @param savepng Logical. Should save a png version of the plot? Default \code{FALSE}.

#' @return Graph of positive cases of COVID19 by region and save a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @importFrom scales comma
#' @examples
#' g_region(saveplot = FALSE, savepng = TRUE)
#' @name g_region

g_region <- function(saveplot = FALSE,
                     savepng = FALSE){

  if (exists('data_province') == FALSE) {
    stop("data objects are missing, run load_data_covid_dr()")
  }

# Por region --------------------------------------------------------------

df_reg <-
  data_province  %>%
  mutate(Reg = case_when(Province %in% Metropolitan ~ 'Metropolitan',
                         Province %in% North ~ 'North',
                         Province %in% South ~ 'South',
                         Province %in% East ~ 'East',
                         Province %in% NE ~ 'NOESP'))

rep_actual <-
  data_cum$Reports %>% max(na.rm = TRUE)

max_reg <-
  df_reg %>%
  filter(date ==  max(date)) %>%
  group_by(Reg) %>%
  summarise(Cases = sum(Cases)) %>%
  select(Cases) %>%
  max() %>%
  round(digits = -3) %>%
  sum(2000) %>%
  as.integer()

total_positives <-
  df_reg %>%
  filter(date ==  max(date)) %>%
  summarise(Cases = sum(Cases)) %>%
  select(Cases) %>%
  as.integer()

lab_reg <-
  labs(title = "DR: COVID19 cases by region",
       subtitle = paste("Total =", comma(total_positives)),
       caption  = paste0("Source: @fidelmorla with information from special bulletin #",
                         rep_actual,
                         " of @SaludPublicaRD"),
       x = "",
       y = ""
  )


col_reg <- c('#bf0000', #azul
             '#d93316', #naranja,
             '#d96a16', #verde
             '#e3981e', #morado
             'darkgrey')

g_reg <-
  df_reg %>%
  filter(date ==  max(date)) %>%
  group_by(Reg) %>%
  summarise(Cases = sum(Cases)) %>%
  mutate(P = 100 * Cases / sum(Cases)) %>%
  drop_na() %>%
  ggplot(aes(x = reorder(Reg, Cases),
             y = Cases,
             fill = reorder(Reg, -Cases),
             col = reorder(Reg, -Cases))) +
  geom_col() +
  scale_y_continuous(labels = comma,
                     breaks = c(seq(0,max_reg, max_reg / 4)),
                     limits = c(0,max_reg)) +
  geom_text(check_overlap = TRUE,
            size = 4.5,
            hjust = -0.25,
            show.legend = FALSE,
            aes(label = paste0(comma(Cases), " (", round(P,1), "%)"))) +
  coord_flip() +
  scale_fill_manual(values = col_reg) +
  scale_color_manual(values = col_reg) +
  lab_reg +
  drcovidplots::list_themes['t6'] +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(color = rev(col_reg)))

print(g_reg)

if (saveplot == TRUE) {assign('g_reg', g_reg, envir = .GlobalEnv)}

if (savepng == TRUE){

  ggsave(filename = "positives_by_region.png",
         plot = g_reg,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

}
