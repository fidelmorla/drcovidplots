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

  url_data_covid_dr <- paste0('https://github.com/fidelmorla/drcovidplots/',
                              'blob/master/excel_data/data_covid_dr.xlsx?raw=true')

  GET(url_data_covid_dr,
      write_disk("data_covid_dr.xlsx",
                 overwrite = TRUE))

  list_data <- list(

    data_cum =
      read_excel('data_covid_dr.xlsx',
                 sheet = "nc")  %>%
      mutate(date = ymd(date)),

    data_province =
      read_excel('data_covid_dr.xlsx',
                 sheet = "c")  %>%
      mutate(date = mdy(date)),

    data_type =
      read_excel('data_covid_dr.xlsx',
                 sheet = "type"),

    data_sex =
      read_excel('data_covid_dr.xlsx',
                 sheet = "sex"),

    data_density =
      read_excel('data_covid_dr.xlsx',
                 sheet = "den"),

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

