#' @title Incidence and lethality rate by region of COVID19
#' @aliases g_incidence_region
#' @description This function graphs the incidence and lethality by region of the COVID19 in the DR.
#' @usage g_incidence_region(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.
#' @param savepng Logical. Should save a png version of the plot? Default \code{FALSE}.
#' @return Graph of incidence and case fatality by region and save a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @importFrom scales comma
#' @export
#' @examples
#' g_incidence_region()
#' g_incidence_region(saveplot = FALSE, savepng = TRUE)
#' @name g_incidence_region

g_incidence_region <- function(saveplot = FALSE,
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
  filter(Cases != 0) %>%
  drop_na(Incidence)


heatcol_inc <- sequential_hcl(n = 32,
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

rep_actual <- data_cum$Reports %>% max(na.rm = TRUE)

max_inc_reg <-
  df_inc %>%
  summarise(max_inc = max(Incidence, na.rm = TRUE)) %>%
  as.numeric() %>%
  round(digits = -1) %>%
  sum(15)

max_let_reg <-
  df_inc %>%
  summarise(max_inc = max(Let, na.rm = TRUE)) %>%
  as.numeric() %>%
  round(digits = -1) %>%
  sum(10)

lab_inclet_reg <-
  labs(title = "DR: COVID19 incidence and lethality by region and province",
       subtitle  =  paste0('Incidence = Positive cases per 100,000 inhabitants',
                           '  /  Let = 100 * Deaths / Positive Cases'),
       caption  = paste0("Source: @fidelmorla with information from special bulletin #",
                         rep_actual,
                         " of @SaludPublicaRD y population reports of @ONERD_"),
       x = "Let",
       y = 'Incidence'
  )


g_inclet_reg <-
  df_inc %>%
  filter(!is.na(Reg)) %>%
  ggplot(aes(x = Let,
             y = Incidence,
             color = as.factor(Reg),
             size = Let,
             fill = as.factor(Reg))) +
  geom_jitter() +
  scale_color_manual(values = heatcol_inc_reg) +
  scale_fill_manual(values = heatcol_inc_reg) +
  scale_y_continuous(labels = scales::comma,
                     breaks = c(seq(0,max_inc_reg, max_inc_reg/ 5)),
                     limits = c(-5,max_inc_reg)) +
  facet_rep_wrap(. ~ fct_inorder(Reg),
                 ncol = 2,
                 repeat.tick.labels = 'all', scales = "fixed")  +
  geom_text_repel(show.legend = FALSE,
                  aes(size = 12,
                      label = case_when(Let > 10 ~ Province,
                                        Incidence > 100 ~ Province)),
                  hjust = 0.2,
                  vjust = 1.2  ) +
  lab_inclet_reg +
  drcovidplots::list_themes['t6'] +
  theme(axis.text.x = element_text(angle = 0),
        strip.text.x = element_text(color = "white",
                                    face = "bold.italic"),
        strip.text.y = element_text(color = "white",
                                    face = "bold.italic"),
        strip.background = element_rect(color = "white",
                                        fill = "#520041",
                                        size = 0.1,
                                        linetype = "solid")
  )

print(g_inclet_reg) %>% suppressWarnings()

if (saveplot == TRUE) {assign('g_inclet_reg', g_inclet_reg, envir = .GlobalEnv)}

if (savepng == TRUE) {

  ggsave(filename = "inclet_reg.png",
         plot = g_inclet_reg,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

}
