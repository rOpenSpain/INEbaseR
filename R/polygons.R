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

#' @title Get operations by granularity
#' @description This function returns a list of all operations that have a temporal or geographic granularity specified by user
#' @param geographical_granularity (string) geographical granularity
#' @param temporal_granularity (string) temporal granularity
#' @param verbose (boolean) show more information during the process
#' @examples
#' get_operations_by_granularity(geographical_granularity = "PROV")
#' get_operations_by_granularity(temporal_granularity = "Anual")
#' @export
get_operations_by_granularity <- function(geographical_granularity = NULL, temporal_granularity = NULL, verbose = TRUE) {

  if ((!is.null(geographical_granularity)) && (!is.null(temporal_granularity))) {
    stop("You only can specify one of these two parameters (geographical_granularity or temporal_granularity), but not both at the same time.")
  }

  # Check geographical granularity
  if (!is.null(geographical_granularity)) {
    if ((geographical_granularity != "PROV") && (geographical_granularity != "CCAA") && (geographical_granularity != "MUN")) {
      stop("geographical_granularity must be one of these options: PROV, CCAA or MUN.")
    }
  }

  # Check temporal granularity
  if (!is.null(temporal_granularity)) {
    if ((temporal_granularity != "Anual") && (temporal_granularity != "Mensual") && (temporal_granularity != "Trimestral")) {
      stop("temporal_granularity must be one of these options: Anual, Mensual or Trimestral")
    }
  }

  operations <- NULL
  all_operations <- get_operations_all()
  for (operation in all_operations$Codigo) {
    # Operation "ETR" (334): Error in open.connection(con, "rb") : HTTP error 500.
    if (operation != "ETR") {

      if (is.null(geographical_granularity)) {
        # Temporal granularity
        series <- get_series_operation(operation)
        if ((length(series$Periodicidad$Nombre) > 0) && (!is.null(series$Periodicidad$Nombre))) {
          if (temporal_granularity %in% series$Periodicidad$Nombre) {
            operations <- c(operations, operation)
            if (verbose) {
              print(paste0("Found (", temporal_granularity, ") in operation: ", operation))
            }
          }
        }
      } else {
        # Geographical granularity
        variables <- get_variables_operation(operation)
        if (geographical_granularity %in% variables$Codigo) {
          operations <- c(operations, operation)
          if (verbose) {
            print(paste0("Found (", geographical_granularity, ") in operation: ", operation))
          }
        }
      }

    }
  }

  return(operations)

}
