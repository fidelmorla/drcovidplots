#' @title Distribution by status of affected individuals of COVID19 in the Dominican Republic
#' @aliases g_status
#' @description This function graphs the distribution according to the status of affected individuals. This can be;
#' 1) Home isolation,
#' 2) Hospital isolation,
#' 3) Recovered and
#' 4) Deaths.
#' @param saveplot Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.
#' @param savepng Logical. Should save a png version of the plot? Default \code{FALSE}.
#' @importFrom scales comma
#' @usage g_status(saveplot = FALSE, savepng = FALSE)
#' @return Graph of the distribution according to the status of affected individuals and saves a
#' copy in png format to the computer at the address defined in \code{setwd()}.
#' @export
#' @examples
#' g_status()
#' @name g_status

g_status <- function(saveplot = FALSE,
                     savepng = FALSE){
    if (exists('data_province') == FALSE) {
      stop("data objects are missing, run load_data_covid_dr()")
    }


    df_type <- data_type

    rep_actual <- data_cum$Reports %>% max(na.rm = TRUE)

    lab_type <-
      labs(title = "DR: Distribution by status of affected individuals of COVID19",
           subtitle = paste("Total =", comma(sum(df_type$N))),
           caption  = paste0("Source: @fidelmorla with information from special bulletin #",
                             rep_actual,
                             " of @SaludPublicaRD"),
           x = "",
           y = ""
      )

    max_type <- 100

    col_type <- c('#038024', #verde
                  '#035d91', #azul
                  '#c47e04', #naranja,
                  '#ad0219') #rojo

    names(col_type) <- c('Recovered', #verde
                         'Home', #azul
                         'Hospital', #naranja,
                         'Deaths') #rojo

    suppressWarnings(
      g_type <-
      df_type %>%
      ggplot(aes(x = reorder(Type, N_p),
                 y = N_p,
                 fill = reorder(Type, -N_p),
                 col = reorder(Type, -N_p))) +
      geom_col() +
      scale_y_continuous(breaks = c(seq(0,max_type, max_type / 4)),
                         limits = c(0,max_type),
                         labels = comma) +
      geom_text(check_overlap = TRUE,
                size = 4.5,
                hjust = -0.25,
                show.legend = FALSE,
                aes(label = paste0(round(N_p,1), " (", comma(N), ")"))) +
      coord_flip() +
      scale_fill_manual(values = col_type) +
      scale_color_manual(values = col_type) +
      lab_type +
      drcovidplots::list_themes['t6'] +
      theme(axis.text.x = element_text(angle = 0),
            axis.text.y = element_text(color = rev(col_type)))
      )

    print(g_type) %>% suppressWarnings()

    if (saveplot == TRUE) {assign("g_type", g_type, envir = .GlobalEnv)}

    if (savepng == TRUE){
      ggsave(filename = "status.png",
             plot = g_type,
             device = "png",
             width = 18.333333333333332 / 1.5,
             height = 10.466666666666667 / 1.5,
             units = "in")
    }

  }
