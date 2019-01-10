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

get_operations_with_granularity <- function(geographical_granularity = NULL, temporal_granularity = NULL) {

  # Check geographical granularity
  if (!is.null(geographical_granularity)) {
    if ((geographical_granularity != "PROV") && (geographical_granularity != "CCAA") && (geographical_granularity != "MUN")) {
      stop("geographical_granularity must be one of these options: PROV, CCAA or MUN.")
    }
  }

  # Check temporal granularity
  if (!is.null(temporal_granularity)) {
    if ((temporal_granularity != "Anual") && (temporal_granularity != "mensual") && (temporal_granularity != "trimestral")) {
      stop("temporal_granularity must be one of these options: anual, mensual or trimestral")
    }
  }

  operations <- NULL
  all_operations <- get_operations_all()
  for (operation in all_operations$Codigo) {
    # Operation "ETR" (334): Error in open.connection(con, "rb") : HTTP error 500.
    if (operation != "ETR") {
      # Temporal granularity
      series <- get_series_operation(operation)
      # series$Periodicidad$Nombre
      # Geographical granularity
      variables <- get_variables_operation(operation)
      if (geographical_granularity %in% variables$Codigo) {
        operations <- c(operations, operation)
        print(paste0("Found (", geographical_granularity, ") in: ", operation))
      }
    }
  }

  return(operations)

}
