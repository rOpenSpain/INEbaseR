# API INE (Polygons)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

get_polygon_file_name <- function(geographical_granularity) {
  directory_root <- get_cache_directory_path(path = "data/geojson")
  file_name <- paste0(directory_root, "/", geographical_granularity, ".geojson")
  return(file_name)
}

draw_serie <- function(serie, geographical_granularity) {

  map <- get_polygon_file_name(geographical_granularity)

  highchart(type = "map") %>%
    hc_add_series(mapData = map, showInLegend = FALSE) %>%
    hc_add_series(
      data = NA, type = "serie",
      dataLabels = list(enabled = FALSE),
      name = "Airports",
      tooltip = list(pointFormat = "{point.name}")
    )
}
