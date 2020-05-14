#' @title Provinces with most positive cases of COVID19
#' @aliases g_case_province
#' @description This function creategraphs the provinces with the most positive cases of COVID19 in the DR.
#' @usage g_case_province()
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.
#' @return Graph of the provinces with the most positive cases of COVID19 and saves a
#' copy in png and gif format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_case_provinces()
#' g_case_provinces(savepng = TRUE)
#' @name g_cases_province
#' @section

g_cases_province <- function(savepng = FALSE,
                             n_province = 15){

    if (exists('data_province') == FALSE) {
      stop("data_province is not present, run load_data_covid_dr()")
    }

    if (exists('data_cum') == FALSE) {
      stop("data_cum is not present, run load_data_covid_dr()")
    }
  if (exists('t3') == FALSE) {
    stop("Themes are not present, run load_themes()")
  }


df_prov_s <-
  data_province  %>%
  mutate(Reg = case_when(Province %in% Metropolitan ~ 'Metropolitan',
                         Province %in% North ~ 'North',
                         Province %in% South ~ 'South',
                         Province %in% East ~ 'East')) %>%
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
         rank <= n_province) %>%
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
  labs(title = paste0('DR: Top ',
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
  scale_y_continuous(breaks = c(seq(0,max_prov_s,max_prov_s/4)),
                     limits = c(0,max_prov_s + 100)) +
  geom_text(aes(size = 15, label = sprintf("%d", round(N, digits = 0))),
            hjust = -1.1, show.legend = FALSE) +
  scale_fill_manual(values = heatcol_s) +
  scale_color_manual(values = heatcol_s) +
  coord_flip() +
  lab_prov_s +
  theme_clean() +
  t6 +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(color = rev(heatcol_s)))

assign('g_cases_prov', g_cases_prov, envir = .GlobalEnv)

# Top ten animado ---------------------------------------------------------

# df_prov_s_f <-
# data_province  %>%
#   mutate(Reg = case_when(Province %in% Metropolitan ~ 'Metropolitan',
#                          Province %in% North ~ 'North',
#                          Province %in% South ~ 'South',
#                          Province %in% East ~ 'East')) %>%
#   gather(key = "covid",
#          value = "N",
#          -date, -Province, -Reg) %>%
#   filter(covid == 'Cases') %>%
#   group_by(date) %>%
#   mutate(rank = rank(-N, ties.method = "random"),
#          Value_rel = rank/max(rank),
#          Value_lbl = paste0("",N)) %>%
#   group_by(Province) %>%
#   filter(rank <= n_province)
#
# heatcol_top15 <- rainbow(n = 32, v = 0.75)
#
# heatcol_top15_reg <-
#   c('#0b559e', #azul
#     '#b50012', #rojo
#     '#03996f', #verde
#     '#805511') #marron
#
# g_prov_s_f <-
#   df_prov_s_f %>%
#   ggplot(aes(rank,
#              group = Province,
#              fill = as.factor(Province),
#              color = as.factor(Province))) +
#   geom_tile(aes(y = N/2,
#                 height = N,
#                 width = 0.9), color = NA) +
#   geom_text(aes(y = 0,
#                 label = paste(Province, " ")),
#             vjust = 0.2,
#             hjust = 1) +
#   geom_text(aes(y = N,
#                 label = Value_lbl,
#                 hjust = -0.75)) +
#   coord_flip(clip = "off",
#              expand = FALSE) +
#   scale_x_reverse() +
#   guides(color = FALSE, fill = FALSE) +
#   scale_color_manual(values = heatcol_top15) +
#   scale_fill_manual(values = heatcol_top15) +
#   t_t10 +
#   transition_states(date,
#                     transition_length = 30,
#                     state_length = 15,
#                     wrap = FALSE) +
#   view_follow(fixed_x = TRUE)  +
#   enter_fade() +
#   exit_fade() +
#   ease_aes('quintic-in') +
#   lab_prov_s
#
# assign("g_cases_province_anim", g_prov_s_f, envir = .GlobalEnv)

# animate(g_prov_s_f,
#         nframes = 200,
#         width = 1100/1.5,
#         height = 628/1.5,
#         end_pause = 30)

if (savepng == TRUE) {
  ggsave(filename = paste0("top_",n_province,".png"),
         plot = g_cases_prov,
         device = "png",
         width = 18.333333333333332 / 1.5,
         height = 10.466666666666667 / 1.5,
         units = "in")

  # anim_save(filename = "g_cases_province_anim.gif",
  #           animation = g_prov_s_f)
}

return(print(.GlobalEnv$g_cases_prov))

}
