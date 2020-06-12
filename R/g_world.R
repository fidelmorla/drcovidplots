#' @title Comparison of the COVID19 situation in the Dominican Republic and the world.
#' @aliases g_world
#' @description This function displays various charts comparing the COVID19 situation
#' in the Dominican Republic and the world.
#' @param savepng Logical. Should save a png version of the plot? Default FALSE.

#' @usage g_world(saveplot = FALSE, savepng = FALSE)
#' @return The following scatter graphics are saved in png format on the computer
#' in the address defined in \code{setwd()}:
#' 1. Lethality against deaths (World and LA)
#' 2. Lethality and positivity (LA)
#' 3. L2 (\code{Deaths / \[Deaths + Recovered\]}) and positive of COVID19 (World and LA)
#' 4. Positives and positivity (World and LA)
#' 5. Positives and recovery rate (World and LA)
#' @export
#' @examples
#' g_world(saveplot = TRUE, savepng = TRUE)
#' @name g_world


g_world <- function(saveplot = FALSE,
                    savepng = FALSE) {


world <- c('Dominican Republic',
           'Italy',
           'Spain',
           'USA',
           'France',
           'China',
           'S. Korea')

la <- c('Chile', 'Bolivia', 'Cuba',
        'Ecuador', 'Uruguay', 'Paraguay',
        'Mexico', 'Guatemala', 'El Salvador',
        'Argentina', 'Brazil', 'Costa Rica',
        'Panama', 'Dominican Republic',
        'Colombia', 'Peru', 'Honduras')

headers_w <-
  c('n',
    'country',
    'cases',
    'n_cases',
    'deaths',
    'n_deaths',
    'recovered',
    'act_cases',
    'serious',
    'cases_1m',
    'deaths_1m',
    'test',
    'test_1m',
    'population',
    'continent')

url_w <- "https://www.worldometers.info/coronavirus/#countries"

html_w <-
  url_w %>%
  xml2::read_html() %>%
  html_nodes("table")

df_w <-
  html_table(html_w[[1]]) %>%
  set_names(nm = headers_w) %>%
  filter(country != 'Total:',
         country != '') %>%
    mutate_at(.vars = headers_w,
            .funs = ~ str_replace_all(string = .,
                                         pattern = ",",
                                         replacement = "")) %>%
    select(-n) %>%
  mutate_at(.vars = vars('cases',
                         'n_cases',
                         'deaths',
                         'n_deaths',
                         'recovered',
                         'act_cases',
                         'serious',
                         'cases_1m',
                         'deaths_1m',
                         'test',
                         'test_1m',
                         'population'),
            .funs = as.numeric) %>%
  select(continent, country, everything()) %>%
    rename(Pop = population) %>%
    drop_na(continent) %>%
    mutate(Pos_1m = 100 * cases_1m / test_1m,
         Pos = 100 * cases / test,
         CFR = 100 * deaths / cases,
         Pos_p = 100 * Pos / Pop,
         world = ifelse(country %in% world, country, NA),
         col_world = ifelse(country %in% world, country, 'grey'),
         la = ifelse(country %in% la, country, NA),
         col_la = ifelse(country %in% la, country, 'grey'),
         alto = ifelse(CFR > 15, country, NA),
         col_lato = ifelse(CFR > 15, country,'grey'))

# CFR & log(deaths) -------------------------------------------------------

lab_w <-
  labs(title = "World: COVID-19 Case fatality rate (CFR) and deaths",
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "Log(Deaths)",
       y = "CFR"
  )


g_w <-
  df_w %>%
  ggplot(aes(x = log(deaths),
             fill = world,
             y = CFR,
             size = CFR)) +
  geom_point(aes(col = col_world)) +
  geom_point(aes(y = ifelse(CFR > 15, CFR, NA), col = 'red')) +
  geom_text_repel(aes(label = world, col = col_world),
                  size = 3,
                  vjust = -1.5,
                  position = position_jitter()) +
  geom_text_repel(aes(label = alto),
                  size = 3, col = 'red',
                  vjust = -1.5, position = position_jitter()) +
  scale_color_manual(values = c('#a39907', #China
                                'black', #DR
                                'darkgreen', #Fra
                                'lightgrey', #NA
                                '#700d75', #ITA
                                'red', #alto
                                '#024d3c', #Korea
                                'darkblue', #Spain
                                'darkred')) + #USA
  lab_w +
  t6 +
  theme(axis.text.x = element_text(angle = 0))

assign('g_w', g_w, envir = .GlobalEnv)

lab_la <-
  labs(title = "Latin America: Case fatality rate (CFR) and deaths from COVID-19",
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "Log(Deaths)",
       y = "CFR"
  )

g_la <-
  df_w %>%
  ggplot(aes(x = log(deaths),
             fill = la,
             y = CFR,
             size = CFR)) +
  geom_point(aes(col = col_la)) +
  geom_text_repel(aes(label = la, col = col_la),
                  size = 3, vjust = -1.5, position = position_jitter()) +
  scale_color_manual(values = c('brown', #ARG
                                'orange', #BOL
                                'darkgreen', # BRA
                                '#056b5c', #CHI
                                'royalblue', #COL
                                'sienna', #COS
                                '#8fa31f', #CUB
                                'black', #DR
                                '#a54fc4', #ECU
                                '#c44f6e', #SAL
                                'lightgrey', #NA
                                'magenta', #Gua
                                '#8f0488', #HON
                                'red', #MEX
                                '#7959c2', #PAN
                                '#a39907', #PAR
                                '#73163d', # PER
                                'navy')) + #URU
  lab_la +
  t6 +
  theme(axis.text.x = element_text(angle = 0))

if (saveplot == TRUE) {assign('g_la', g_la, envir = .GlobalEnv)}

if (savepng == TRUE){


ggsave(filename = "world.png",
       plot = g_w,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")

ggsave(filename = "la.png",
       plot = g_la,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}

# Comparacion pos - def ---------------------------------------------------

lab_la_pp <-
  labs(title = "Latin America: COVID-19 Case fatality and positive rates",
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "CFR",
       y = "Positive / Cases"
  )

g_let_per <-
  df_w %>%
  ggplot(aes(x = CFR, fill = la, y = log(Pos))) +
  geom_point(aes(col = col_la, size = CFR)) +
  geom_text_repel(aes(label = la, col = col_la),
                  size = 3, vjust = -1.5, position = position_jitter()) +
  scale_color_manual(values = c('brown', #ARG
                                'orange', #BOL
                                'darkgreen', # BRA
                                '#056b5c', #CHI
                                'royalblue', #COL
                                'sienna', #COS
                                '#8fa31f', #CUB
                                'black', #DR
                                '#a54fc4', #ECU
                                '#c44f6e', #SAL
                                'lightgrey', #NA
                                'magenta', #Gua
                                '#8f0488', #HON
                                'red', #MEX
                                '#7959c2', #PAN
                                '#a39907', #PAR
                                '#73163d', # PER
                                'navy')) + #URU
  lab_la_pp +
  t6 +
  theme(axis.text.x = element_text(angle = 0))

if (saveplot == TRUE) {assign('g_let_per', g_let_per, envir = .GlobalEnv)}

if (savepng == TRUE){

ggsave(filename = "let_per.png",
       plot = g_let_per,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}
# Letalidad de Ghani ------------------------------------------------------

df_w_letg <-
  df_w %>%
  mutate(letg = 100 * deaths / (deaths + recovered)) %>%
  mutate(world = ifelse(country %in% world, country, NA),
         col_world = ifelse(country %in% world, country, 'grey'),
         al = ifelse(country %in% la, country, NA),
         col_la = ifelse(country %in% la, country, 'grey'),
         alto = ifelse(letg > 75, country,NA),
         col_lato = ifelse(letg > 75, country,'grey'))


lab_w_letg <-
  labs(title = paste0("World: Comparison of COVID-19 Positive cases ",
                      "with lethality by Ghani et al. (2005)"),
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "Log(Positives)",
       y = "Let = Deaths / (Deaths + Recovered)"
  )


g_w_letg <-
  df_w_letg %>%
  filter(log(Pos) > 0) %>%
  ggplot(aes(x = log(Pos),
             fill = world,
             y = letg,
             size = letg)) +
  geom_point(aes(col = col_world)) +
  geom_point(aes(y = ifelse(letg > 75,
                            letg,
                            NA),
                 col = 'red')) +
  geom_text(aes(label = world,
                col = col_world),
            size = 3,
            vjust = -1.5,
            position = position_jitter()) +
  geom_text(aes(label = alto),
            size = 3,
            col = 'red',
            vjust = -1.5,
            position = position_jitter()) +
  scale_color_manual(values = c('black', #DR
                                'darkgreen', #FRA
                                'lightgrey', #NA
                                'royalblue', #ITA
                                'red', #HIGH
                                'brown', #KOR
                                'darkblue', #SPA
                                'darkred', #USA
                                'purple')) + #alto
  lab_w_letg +
  t6 +
  theme(axis.text.x = element_text(angle = 0))

assign('g_w_letg', g_w_letg, envir = .GlobalEnv)


lab_la_letg <-
  labs(title = paste0("Latin America: Comparison of COVID-19 Positive cases ",
                      "with lethality by Ghani et al. (2005)"),
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "Log(Positives)",
       y = "Let = Deaths / (Deaths + Recovered)"
  )


g_la_letg <-
  df_w_letg %>%
  filter(log(Pos) > 0) %>%
  ggplot(aes(x = log(Pos), fill = la, y = letg, size = letg)) +
  geom_point(aes(col = col_la)) +
  geom_text_repel(aes(label = la, col = col_la),
                  size = 3, vjust = -1.5, position = position_jitter()) +
  scale_color_manual(values = c('brown', #ARG
                                'orange', #BOL
                                'darkgreen', # BRA
                                '#056b5c', #CHI
                                'royalblue', #COL
                                'sienna', #COS
                                '#8fa31f', #CUB
                                'black', #DR
                                '#a54fc4', #ECU
                                '#c44f6e', #SAL
                                'lightgrey', #NA
                                'magenta', #Gua
                                '#8f0488', #HON
                                'red', #MEX
                                '#7959c2', #PAN
                                '#a39907', #PAR
                                '#73163d', # PER
                                'navy')) + #URU
  lab_la_letg +
  t6 +
  theme(axis.text.x = element_text(angle = 0))

if (saveplot == TRUE) {assign('g_la_letg', g_la_letg, envir = .GlobalEnv)}

if (savepng == TRUE){

ggsave(filename = "world_letg.png",
       plot = g_w_letg,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")

ggsave(filename = "la_letg.png",
       plot = g_la_letg,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}
# Positivos vs pruebas realizadas -----------------------------------------

lab_w_pos_r <-
  labs(title = "World: COVID-19 positive cases",
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "Log(Positive)",
       y = "Positive / Tests"
  )

g_w_pos_r <-
  df_w %>%
  ggplot(aes(x = log(cases),
             fill = world,
             y = Pos,
             size = Pos)) +
  geom_point(aes(col = col_world)) +
  geom_point(aes(y = ifelse(Pos > 35, Pos, NA), col = 'red')) +
  geom_text_repel(aes(label = world, col = col_world),
                  size = 3,
                  vjust = -1.5,
                  position = position_jitter()) +
  geom_text_repel(aes(label = ifelse(Pos > 35, country, NA)),
                  size = 3, col = 'red',
                  vjust = -1.5, position = position_jitter()) +
  scale_color_manual(values = c('#a39907', #China
                                'black', #DR
                                'darkgreen', #Fra
                                'lightgrey', #NA
                                '#700d75', #ITA
                                'red', #alto
                                '#024d3c', #Korea
                                'darkblue', #Spain
                                'darkred')) + #USA
  lab_w_pos_r +
  t6 +
  theme(axis.text.x = element_text(angle = 0))

assign('g_w_pos_r', g_w_pos_r, envir = .GlobalEnv)


lab_la_pos_r <-
  labs(title = "Latin America: COVID-19 positive cases",
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "Log(Positive)",
       y = "Positive / Tests"
  )


g_la_pos_r <-
  df_w %>%
  ggplot(aes(x = log(cases),
             fill = la,
             y = Pos,
             size = Pos)) +
  geom_point(aes(col = col_la)) +
  geom_point(aes(y = ifelse(Pos > 35, Pos, NA), col = 'red')) +
  geom_text_repel(aes(label = la, col = col_la),
                  size = 3,
                  vjust = -1.5,
                  position = position_jitter()) +
  geom_text_repel(aes(label = ifelse(Pos > 35, country, NA)),
                  size = 3, col = 'red',
                  vjust = -1.5, position = position_jitter()) +
  scale_color_manual(values = c('brown', #ARG
                                'blue', #BOL
                                'orange', #BRA
                                'darkgreen', #CHI
                                '#056b5c', #CUB
                                'red 2', #GUA
                                'darkgoldenrod',
                                'black', #DR
                                'green 4', #ECU
                                '#4d8a1e', #SAL
                                'lightgrey', #NA
                                'purple 3', #
                                'magenta', #HON
                                '#8f0488', #MEX
                                'blue 3', #PAN
                                '#7959c2', #PAR
                                '#a39907', #PER
                                'red', # RD
                                'navy')) + #URU
  lab_la_pos_r +
  t6 +
  theme(axis.text.x = element_text(angle = 0))

if (saveplot == TRUE) {assign('g_la_pos_r', g_la_pos_r, envir = .GlobalEnv)}

if (savepng == TRUE){

ggsave(filename = "world_pr.png",
       plot = g_w_pos_r,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")

ggsave(filename = "la_pr.png",
       plot = g_la_pos_r,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")

}
# Tasa de Recuperacion  ----------------------------------------------

df_w_rec <-
  df_w %>%
  mutate(rec_r = 100 * recovered / cases) %>%
  mutate(world = ifelse(country %in% world, country, NA),
         col_world = ifelse(country %in% world, country, 'grey'),
         al = ifelse(country %in% la, country, NA),
         col_la = ifelse(country %in% la, country, 'grey'))

lab_w_rec <-
  labs(title = "World: COVID-19 recovery rate",
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "Log(Positive)",
       y = "Recovery rate = Recovered / Positive"
  )


lab_la_rec <-
  labs(title = "Latin America: COVID-19 recovery rate",
       subtitle  =  paste0("Up to ",
                           Sys.Date()  %>%
                             format(format = '%B %d, %Y')),
       caption  = paste0("Source: @fidelmorla with information retrieved",
                         " from https://www.worldometers.info/coronavirus/#countries"),
       x = "Log(Positive)",
       y = "Recovery rate = Recovered / Positive"
  )


g_w_rec <-
  df_w_rec %>%
  ggplot(aes(x = log(cases),
             fill = world,
             y = rec_r,
             size = rec_r)) +
  geom_point(aes(col = col_world)) +
  geom_text_repel(aes(label = world, col = col_world),
                  size = 3,
                  vjust = -1.5,
                  position = position_jitter()) +
  scale_color_manual(values = c('green 3', #China
                                'black', #DR
                                'blue', #FRA
                                'lightgrey', #NA
                                'red 2', #alto
                                'steelblue', #Korea
                                'brown', #Spain
                                'purple')) + #USA
  lab_w_rec +
  t6 +
  theme(axis.text.x = element_text(angle = 0))

assign('g_w_rec', g_w_rec, envir = .GlobalEnv)

g_la_rec <-
  df_w_rec %>%
  ggplot(aes(x = log(cases),
             fill = la,
             y = rec_r,
             size = rec_r)) +
  geom_point(aes(col = col_la)) +
  geom_text_repel(aes(label = la, col = col_la),
                  size = 3,
                  vjust = -1.5,
                  position = position_jitter()) +
  scale_color_manual(values = c('brown', #ARG
                                'blue', #BOL
                                'orange', #BRA
                                '#056b5c', #CUB
                                'red 2', #GUA
                                'darkgoldenrod',
                                'green 3', #DR
                                'black', #ECU
                                '#4d8a1e', #SAL
                                'purple', #NA
                                'lightgrey', #
                                'magenta', #HON
                                '#8f0488', #MEX
                                'blue 3', #PAN
                                '#7959c2', #PAR
                                '#a39907', #PER
                                'red', # RD
                                'navy')) + #URU
  lab_la_rec +
  t6 +
  theme(axis.text.x = element_text(angle = 0))


if (saveplot == TRUE) {assign('g_la_rec', g_la_rec, envir = .GlobalEnv)}

if (savepng == TRUE){

ggsave(filename = "world_rec.png",
       plot = g_w_rec,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")

ggsave(filename = "la_rec.png",
       plot = g_la_rec,
       device = "png",
       width = 18.333333333333332 / 1.5,
       height = 10.466666666666667 / 1.5,
       units = "in")
}

}
