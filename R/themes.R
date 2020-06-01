#' @title Charts themes
#'
#' @description This function loads all themes used in the package.
#'
#' @return Objects class \code{theme()} containing the themes to use in the charts functions.

#' @export
#'
#' @examples
#' load_themes()

#' @name load_themes
#'
#'

# t3 ------------------------------------------------------------------

load_themes <- function(){

   list_themes <- list(

t3 =
theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major.x = element_line( size=.1, color="grey" ),
      panel.grid.minor.x = element_line( size=.1, color="grey" ),
      plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
      plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
      plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
      plot.background=element_blank(),
      plot.margin = margin(2,2, 2, 4, "cm")),


# t4 -------------------------------------------------------------------


t4 =
   theme_minimal() +
   theme(axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position="none",
         panel.background=element_blank(),
         panel.border=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         panel.grid.major.x = element_line( size=.1, color="grey" ),
         panel.grid.minor.x = element_line( size=.1, color="grey" ),
         plot.title=element_text(size=25, hjust=0.5, face="bold",     colour="red", vjust=-1),
         plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="red"),
         plot.caption =element_text(size=12, hjust=0.5, face="italic", color="red"),
         plot.background=element_blank(),
         plot.margin = margin(1,4, 1, 8, "cm")),


# t5 -------------------------------------------------------------------


t5 =   theme_clean() +
   theme(plot.title = element_text(color = rgb(.35,.35,.35)),
         plot.subtitle = element_text(color = rgb(.35,.35,.35)),
         plot.caption = element_text(color = rgb(.55,.55,.55)),
         axis.title.x = element_text(color = rgb(.55,.55,.55)),
         axis.title.y = element_text(color = rgb(.55,.55,.55)),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 0,
                                    color = rgb(.55,.55,.55)),
         axis.text.y = element_text(color = rgb(.55,.55,.55)),
         axis.line.x = element_line(colour = rgb(.55,.55,.55)),
         axis.line.y = element_line(colour = rgb(.55,.55,.55)),
         legend.position = "none",
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_blank()
   ),


# t 6 ------------------------------------------------------------------


t6 =   theme_clean() +
   theme(plot.title = element_text(hjust = 0,
                                   color = rgb(.35,.35,.35)),
         plot.subtitle = element_text(hjust = 0,
                                      color = rgb(.35,.35,.35)),
         plot.caption = element_text(hjust = 0,
                                     color = rgb(.35,.35,.35)),
         axis.title.x = element_text(color = rgb(.55,.55,.55)),
         axis.title.y = element_text(color = rgb(.55,.55,.55)),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90,
                                    color = rgb(.55,.55,.55)),
         axis.text.y = element_text(color = rgb(.55,.55,.55)),
         axis.line.x = element_line(colour = rgb(.55,.55,.55)),
         axis.line.y = element_line(colour = rgb(.55,.55,.55)),
         legend.position = "none",
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_blank(),
strip.text.x = element_text(color = "#0952ab",
                            face = "bold.italic"),
strip.text.y = element_text(color = "#0952ab",
                            face = "bold.italic"),
strip.background = element_rect(color = "white",
                                fill = "white",
                                size = 1,
                                linetype = "solid")
),


# Darktheme ---------------------------------------------------------------

t_darkblue =   theme_clean() +
   theme(plot.title = element_text(hjust = 0,
                                   color = 'white'),
         plot.subtitle = element_text(hjust = 0,
                                      color = 'white'),
         plot.caption = element_text(hjust = 0,
                                     color = 'white'),
         plot.background = element_rect(fill = '#01264d'),
         #panel.background = element_rect(fill = '#01264d'),
         axis.title.x = element_text(
            color = 'white'),
         axis.title.y = element_text(
            color = 'white'),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90,
                                    color = 'white'),
         axis.text.y = element_text(
            color = 'white'),
         axis.line.x = element_line(
            color = 'white'),
         axis.line.y = element_line(
            color = 'white'),
         legend.position = "none",
         panel.grid.major.y = element_line(
            color = 'white'),
         panel.grid.minor.x = element_blank(),
         strip.text.x = element_text(
            color = 'white',
         face = "bold.italic"),
         strip.text.y = element_text(color = "#0952ab",
                                     face = "bold.italic"),
         strip.background = element_rect(color = "white",
                                         fill = "white",
                                         size = 1,
                                         linetype = "solid")
   ),


t_darkgreen =   theme_clean() +
   theme(plot.title = element_text(hjust = 0,
                                   color = 'white'),
         plot.subtitle = element_text(hjust = 0,
                                      color = 'white'),
         plot.caption = element_text(hjust = 0,
                                     color = 'white'),
         plot.background = element_rect(fill = '#0c4c4f'),
         axis.title.x = element_text(
            color = 'white'),
         axis.title.y = element_text(
            color = 'white'),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90,
                                    color = 'white'),
         axis.text.y = element_text(
            color = 'white'),
         axis.line.x = element_line(
            color = 'white'),
         axis.line.y = element_line(
            color = 'white'),
         legend.position = "none",
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         strip.text.x = element_text(
            color = 'white',
            face = "bold.italic"),
         strip.text.y = element_text(color = "#0952ab",
                                     face = "bold.italic"),
         strip.background = element_rect(color = "white",
                                         fill = "white",
                                         size = 1,
                                         linetype = "solid")
   ),

t_darkred =   theme_clean() +
   theme(plot.title = element_text(hjust = 0,
                                   color = 'white'),
         plot.subtitle = element_text(hjust = 0,
                                      color = 'white'),
         plot.caption = element_text(hjust = 0,
                                     color = 'white'),
         plot.background = element_rect(fill = '#850000'),
         axis.title.x = element_text(
            color = 'white'),
         axis.title.y = element_text(
            color = 'white'),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90,
                                    color = 'white'),
         axis.text.y = element_text(
            color = 'white'),
         axis.line.x = element_line(
            color = 'white'),
         axis.line.y = element_line(
            color = 'white'),
         legend.position = "none",
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         strip.text.x = element_text(
            color = 'white',
            face = "bold.italic"),
         strip.text.y = element_text(color = "white",
                                     face = "bold.italic"),
         strip.background = element_rect(color = "white",
                                         fill = "white",
                                         size = 1,
                                         linetype = "solid")
   ),


t_darkorange =   theme_clean() +
   theme(plot.title = element_text(hjust = 0,
                                   color = 'white'),
         plot.subtitle = element_text(hjust = 0,
                                      color = 'white'),
         plot.caption = element_text(hjust = 0,
                                     color = 'white'),
         plot.background = element_rect(fill = '#c47e04'),
         axis.title.x = element_text(
            color = 'white'),
         axis.title.y = element_text(
            color = 'white'),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90,
                                    color = 'white'),
         axis.text.y = element_text(
            color = 'white'),
         axis.line.x = element_line(
            color = 'white'),
         axis.line.y = element_line(
            color = 'white'),
         legend.position = "none",
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         strip.text.x = element_text(
            color = 'white',
            face = "bold.italic"),
         strip.text.y = element_text(color = "white",
                                     face = "bold.italic"),
         strip.background = element_rect(color = "white",
                                         fill = "white",
                                         size = 1,
                                         linetype = "solid")
   ),


# t con leyenda --------------------------------------------------------

t_legend =   theme_clean() +
   theme(plot.title = element_text(hjust = 0,
                                   color = rgb(.35,.35,.35)),
         plot.subtitle = element_text(hjust = 0,
                                      color = rgb(.35,.35,.35)),
         plot.caption = element_text(hjust = 0,
                                     color = rgb(.35,.35,.35)),
         axis.title.x = element_text(color = rgb(.55,.55,.55)),
         axis.title.y = element_text(color = rgb(.55,.55,.55)),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90,
                                    color = rgb(.55,.55,.55)),
         axis.text.y = element_text(color = rgb(.55,.55,.55)),
         axis.line.x = element_line(colour = rgb(.55,.55,.55)),
         axis.line.y = element_line(colour = rgb(.55,.55,.55)),
         legend.position = c(0.2, 0.75),
         legend.background = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         strip.text.x = element_text(color = "#0952ab",
                                     face = "bold.italic"),
         strip.text.y = element_text(color = "#0952ab",
                                     face = "bold.italic"),
         strip.background = element_rect(color = "white",
                                         fill = "white",
                                         size = 1,
                                         linetype = "solid")
   ),




# t T10 ----------------------------------------------------------------

t_t10 =
   theme_clean() +
   theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(size = 0.1,
                                        color = "lightgrey" ),
      panel.grid.minor.x = element_line(size = 0.1,
                                        color = "lightgrey"),
      plot.title = element_text(hjust = 0,
                                color = rgb(.35,.35,.35)),
      plot.subtitle = element_text(hjust = 0,
                                   color = rgb(.35,.35,.35)),
      plot.caption = element_text(hjust = 0,
                                  color = rgb(.35,.35,.35)),
      plot.background = element_blank(),
      plot.margin = margin(t = 2,
                           r = 2,
                           b = 2,
                           l = 4,
                           unit = "cm")
      )
)

   themes_items <- c('t3',
                     't4',
                     't5',
                     't6',
                     't_darkblue',
                     't_darkgreen',
                     't_darkred',
                     't_darkorange',
                     't_legend',
                     't_t10')

for (i in 1:length(themes_items)) {

   assign(x = as.character(themes_items[i]),
          value = list_themes[[i]],
          envir = .GlobalEnv)

}


}
