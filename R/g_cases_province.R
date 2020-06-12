#' @title Provinces with most positive cases of COVID19
#' @aliases g_cases_province
#' @description This function creategraphs the provinces with the most positive
#'   cases of COVID19 in the DR.
#' @usage g_cases_province(n_province = 15L, saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @param n_province Integer. Number of provinces to show on plot. Range = [2,32].
#'   Default \code{n_province = 15L}.
#' @return Graph of the provinces with the most positive cases of COVID19 and saves a
#' copy in png and gif format to the computer at the address defined in \code{setwd()}.
#' @importFrom scales comma
#' @export
#' @examples
#' g_cases_province()
#' g_cases_province(savepng = TRUE)
#' g_cases_province(n_province = 25L, saveplot = TRUE)
#' @name g_cases_province


g_cases_province <- function(n_province = 15L,
                             saveplot = FALSE,
                             savepng = FALSE) {

    if (exists('data_province') == FALSE) {
      stop("data objects are missing, run load_data_covid_dr()")
    }


  if (n_province < 2) {
    stop("n_province has to be an interger betweem 2 and 32")
  }

  if (n_province > 32) {
    stop("n_province has to be an interger betweem 2 and 32")
  }

  if (as.integer(n_province) == FALSE){
    stop("n_province has to be an interger betweem 2 and 32")
  }


df_prov_s <-
  data_province  %>%
  mutate(Reg = case_when(Province %in% Metropolitan ~ 'Metropolitan',
                         Province %in% North ~ 'North',
                         Province %in% South ~ 'South',
                         Province %in% East ~ 'East',
                         Province %in% NE ~ 'NOESP')) %>%
  gather(key = "covid",
         value = "N",
         -date, -Province, -Reg) %>%
  group_by(date) %>%
  filter(covid == 'Cases') %>%
  mutate(rank = rank(-N, ties.method = "random"),
         Value_rel = rank/max(rank),
         Value_lbl = paste0("",N)) %>%
  ungroup() %>%
  filter(date == max(date),
         rank <= n_province,
         Province != 'NOESP') %>%
  arrange(rank)

max_prov_s <-
  df_prov_s %>%
  summarise(max_N = round(x = max(N),
                          digits = -2)) %>%
  select(max_N) %>%
  as.integer() %>%
  sum(100)

#Colores para el graph-
heatcol_s <- sequential_hcl(n = n_province,
                            palette = "YlOrRd",
                            power = 0.5,
                            l = 30,
                            c = 225,
                            c1 = 150
)

lab_prov_s <-
  labs(title = paste0('DR: Top-',
                      n_province,
                      " provinces affected by COVID-19"),
       caption  = "Source: @fidelmorla with the special bulletins of @SaludPublicaRD",
       x = "",
       y = "Positive"
  )

g_cases_prov <-
  df_prov_s %>%
  ggplot(aes(x = reorder(Province, -rank, order = TRUE),
             y = N,
             fill = reorder(Province, rank),
             col = reorder(Province, rank))) +
  geom_col(width = .8) +
  scale_y_continuous(labels = scales::comma,
                     breaks = c(seq(0,max_prov_s,max_prov_s/4)),
                     limits = c(0,max_prov_s + 500)) +
  geom_text(aes(size = 15,
                label = scales::comma(N, accuracy = 1)),
            hjust = -0.5,
            show.legend = FALSE) +
  scale_fill_manual(values = heatcol_s) +
  scale_color_manual(values = heatcol_s) +
  coord_flip() +
  lab_prov_s +
  theme_clean() +
  list_themes['t6'] +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(color = rev(heatcol_s)))

print(g_cases_prov)

if (saveplot == TRUE) {
assign('g_cases_prov', g_cases_prov, envir = .GlobalEnv)
}

if (savepng == TRUE) {
  ggsave(filename = paste0("top_",n_province,".png"),
         plot = g_cases_prov,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")
}

}
