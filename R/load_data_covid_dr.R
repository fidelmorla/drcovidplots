#' @title Load database from googlesheets and themes for each chart.
#' @description This function loads information contained in data_covid_dr.xlsx about
#' COVID-19 in the Dominican Republic to .GlobalEnv.
#' @return 5 objects class \code{data.frame()} containing the information to use in the graphs functions.
#' Also attached the formats or themes of the graphics to .GlobalEnv.
#' @export
#' @examples
#' load_data_covid_dr()
#' @name load_data_covid_dr

load_data_covid_dr <- function(){

  url_data_cum <- paste0('https://raw.githubusercontent.com/fidelmorla/',
                         'drcovidplots/master/csv_data/data_cum.csv')

  url_data_density <- paste0('https://raw.githubusercontent.com/fidelmorla/',
                         'drcovidplots/master/csv_data/data_density.csv')

  url_data_province <- paste0('https://raw.githubusercontent.com/fidelmorla/',
                             'drcovidplots/master/csv_data/data_province.csv')

  url_data_type <- paste0('https://raw.githubusercontent.com/fidelmorla/',
                              'drcovidplots/master/csv_data/data_type.csv')

  url_data_sex <- paste0('https://raw.githubusercontent.com/fidelmorla/',
                          'drcovidplots/master/csv_data/data_sex.csv')

  list_data <- list(

    data_cum =
      read.csv(url_data_cum,
               sep = ",") %>%
      mutate(date = ymd(date)),

    data_province =
      read.csv(url_data_province,
               sep = ",")  %>%
      mutate(date = ymd(date)),

    data_type =
      read.csv(url_data_type,
               sep = ","),

    data_sex =
      read.csv(url_data_sex,
               sep = ","),

    data_density =
      read.csv(url_data_density,
               sep = ","),

    Metropolitan = c( 'SD',
                      'DN'),

    North = c('ESP',
              'PPLATA',
              'DJBON',
              'DTE',
              'VEGA',
              'MTS',
              'MC',
              'MIRABAL',
              'SAM',
              'SRAM',
              'STGO',
              'SROD',
              'VALV',
              'NOUEL'),

    South = c('AZUA',
              'BAO',
              'BARA',
              'EP',
              'IND',
              'SCBAL',
              'SJUAN',
              'PED',
              'PERV',
              'OCOA'),

    East = c('SEIBO',
             'ALT',
             'ROM',
             'SPM',
             'MPLATA',
             'HMAYOR'),

    NE = c('NOESP')
  )

  rep_actual <-
    list_data %$%
    data_cum %>%
    select(Reports) %>%
    max()

  assign('rep_actual', rep_actual, envir = .GlobalEnv)

  data_items <- c('data_cum',
                  'data_province',
                  'data_type',
                  'data_sex',
                  'data_density',
                  'Metropolitan',
                  'North',
                  'South',
                  'East',
                  'NE')

  for (i in 1:length(data_items)) {

    assign(x = as.character(data_items[i]),
           value = list_data[[i]],
           envir = .GlobalEnv)

  }


}

