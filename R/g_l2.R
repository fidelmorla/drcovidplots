#' @title Case fatality rate (L2) from Ghani \emph{et al.} (2005) of COVID19
#' @aliases g_l2
#' @description This function graphs the L2 of the COVID19 in RD. L2 = 100 * Deaths / (Deaths + Recovered),
#' as recommended by Ghani \emph{et al.} (2005) for the Dominican Republic
#' @usage g_l2(saveplot = FALSE, savepng = FALSE)
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default FALSE.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Graphic of L2 of COVID19 and save a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#'
#' @references A. C. Ghani, C. A. Donnelly, D. R. Cox, J. T. Griffin, C. Fraser, T. H. Lam, L. M. Ho,
#' W. S. Chan, R. M. Anderson, A. J. Hedley, G. M. Leung, Methods for Estimating the Case Fatality
#' Ratio for a Novel, Emerging Infectious Disease, American Journal of Epidemiology, Volume 162,
#' Issue 5, 1 September 2005, Pages 479–486, \url{https://doi.org/10.1093/aje/kwi230}
#' @export
#' @examples
#' g_l2()
#' g_l2(saveplot = TRUE, savepng = FALSE)
#' @name g_l2

g_l2 <- function(saveplot = FALSE,
                 savepng = FALSE){
    if (exists('data_cum') == FALSE) {
      stop("data objects are missing, run load_data_covid_dr()")
    }

# Tasa de letalidad Ghani et al. 2005 -------------------------------------

df_l2 <-
  data_cum  %>%
  mutate(date = ymd(date)) %>%
  filter(Reports > 0) %>%
  select(date, Positive, Deaths, Recovered) %>%
  mutate(Let = 100 * Deaths / Positive,
         Let_ghani = 100 * Deaths / (Deaths + Recovered),
         label_g = ifelse(Let_ghani == Let_ghani[date == max(date)],
                          Let_ghani,
                          NA))


lab_l2 <-
  labs(title = "RD: Lethality from Ghani et al. (2005) of COVID-19",
       subtitle  =  paste0('L2 = Deaths / (Deaths + Recovered)'," %"),
       caption  = paste0("Source: @fidelmorla with information from special bulletins of @SaludPublicaRD"),
       x = "",
       y = ""
  )

g_l2 <-
  df_l2 %>%
  ggplot(aes(x = date, y = Let_ghani)) +
  geom_pointpath(aes(fill = "blue", col = "blue")) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "2 days") +
  scale_y_continuous(breaks = c(seq(0, 100, 25)),
                     limits = c(0,100)) +
  geom_text(show.legend = FALSE,
            aes(col = "blue",
                size = 24,
                label = round(label_g,1)),
            vjust = -0.5) +
  scale_fill_manual(values = c("#0981bd", '#0981bd')) +
  scale_color_manual(values = c("#0981bd", '#0981bd')) +
  theme_clean() +
  drcovidplots::list_themes['t6'] +
  lab_l2

print(g_l2)

if(saveplot == TRUE) {assign('g_let2', g_l2, envir = .GlobalEnv)}

if (savepng == TRUE){
ggsave(filename = "l2.png",
       plot = g_l2,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
  }

}
