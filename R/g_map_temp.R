#install.packages("sf")

# Importando los shapefiles
map_provincias <- sf::read_sf("dr_shapefiles/provincia/PROVCenso2010.shp")
map_regiones <- sf::read_sf("dr_shapefiles/region/REGCenso2010.shp")

# loading covid data
load_data_covid_dr()

# Ajustando variables
plot(map_provincias)


map_provincias <- map_provincias %>%
  dplyr::transmute(
    cod_prov = PROV,
    cod_reg = REG,
    province_name = stringr::str_to_title(TOPONIMIA),
    province_short = recode(province_name,
                            "Azua" = "AZUA",
                            "Baoruco" = "BAO",
                            "Barahona" = "BARA",
                            "Dajabón" = "DJBON",
                            "Distrito Nacional" = "DN",
                            "Duarte" = "DTE",
                            "El Seibo" = "SIBO",
                            "Elías Piña" = "EP",
                            "Espaillat" = "ESP",
                            "Hato Mayor" = "HMAYOR",
                            "Hermanas Mirabal" = "MIRABAL",
                            "Independencia" = "IND",
                            "La Altagracia" = "LA",
                            "La Romana" = "ROM",
                            "La Vega" = "VEGA",
                            "María Trinidad Sánchez" = "MTS",
                            "Monseñor Nouel" = "NOUEL",
                            "Monte Cristi" = "MC",
                            "Monte Plata" = "MPLATA",
                            "Pedernales" = "PED",
                            "Peravia" = "PERV",
                            "Puerto Plata" = "PPLATA",
                            "Samaná" = "SAM",
                            "San Cristóbal" = "SCBAL",
                            "San José De Ocoa" = "OCOA",
                            "San Juan" = "SJUAN",
                            "San Pedro De Macorís" = "SPM",
                            "Sanchez Ramírez" = "SRAM",
                            "Santiago" = "STGO",
                            "Santiago Rodríguez" = "SROD",
                            "Santo Domingo" = "SD",
                            "Valverde" = "VAL",
                        ),
    geometry

  )


save(map_provincias, file =  "data/map_provincias.RData")


map_provincias %>%
  left_join(
    data_province %>%
      filter(date == max(date)),

    by = c("province_short" = "Province")
  ) %>%
  select(everything(), geometry)



p_map <- function(date = "latest", interactive = FALSE) {

  date2 <- date

  `%>%` <- magrittr::`%>%`

  load("data/map_provincias.RData")
  load("data/data_covid_dr.RData")

  if(date == "latest") {
    data <- data_province %>%
      dplyr::filter(date == max(date))
  } else {
    data <- data_province %>%
      dplyr::filter(date == date2)
  }

  if(interactive == FALSE) {

  map <- dplyr::left_join(map_provincias, data, by = c("province_short" = "Province")) %>%
    mutate(
      CENTROID = purrr::map(geometry, sf::st_centroid),
      COORDS = purrr::map(CENTROID, sf::st_coordinates),
      COORDS_X = purrr::map_dbl(COORDS, 1),
      COORDS_Y = purrr::map_dbl(COORDS, 2),
      label_short = paste0(province_short, ": ", Cases)
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = Cases)) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_continuous(
     # high = "#132B43", low = "#56B1F7",
      #breaks = c(1, 1.5, 2, 2.5, 3),
      #labels = c("0", "60", "300", "1,000", "2,000"),
      guide = ggplot2::guide_colorbar(barwidth = 10)
    ) +
    ggplot2::theme(
      legend.position = c(0.5, 0.2),
      legend.direction = "horizontal"
    ) +
    ggrepel::geom_label_repel(ggplot2::aes(COORDS_X, COORDS_Y, label = label_short),
                              size = 3, min.segment.length = 0, point.padding = NA)
  }

  return(map)

}

p_map()



