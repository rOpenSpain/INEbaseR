# API INE (Polygons)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

# Example: get_natcode("IPC251541", 115)
# Example: get_natcode(all)
get_natcode <- function(serie = NULL, variable_id = NULL, all = TRUE) {

  # Natcode
  natcode <- "34" # Spain prefix: 34

  # Get natcode table from cache
  natcode_list <- get_rds_content("natcodes", type = "/DATATABLE-")
  natcode_df <- data.frame(natcode_list)

  # Get all natcode
  if (all) {
    return(natcode_df)
  }

  # Get serie metadata
  serie_metadata <- get_serie(serie, det = 2, tip = "M")

  variable_codigo <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Id == variable_id,]$Variable$Codigo
  variable_id <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Id == variable_id,]$Id

  # Provincias
  if (variable_codigo == "PROV") {
    lista_ccaa <- natcode_df[natcode_df$CPRO == variable_id,]$CODAUTO
    cod_ccaa <- unique(lista_ccaa)

    if (length(cod_ccaa) > 1) {
      cod_ccaa <- cod_ccaa[1]
    }

    natcode <- paste0(natcode, cod_ccaa, variable_id, "000", "00")

  } else {
    # Comunidades autónomas
    if (variable_codigo == "CCAA") {
      natcode <- paste0(natcode, variable_id, "00", "000", "00")
    # Municipios
    } else {
      lista_mun <- natcode_df[natcode_df$CMUN == variable_id,]$CMUN
      natcode <- paste0(natcode, variable_id, "00", variable_id, "00")

    }
  }

  return(natcode)

}

# Example: draw_serie("IPC251541", "provincias")
draw_serie <- function(serie, geographical_granularity, nult = 0) {

  # Message
  message("Note: represent all polygons may take much time, please be patient ...")

  # Get polygon from cache
  map <- get_rds_content(geographical_granularity)

  # Get serie metadata
  serie_metadata <- get_serie(serie, det = 2, tip = "M")
  serie_variables_id <- serie_metadata$MetaData$Variable$Id
  # Variables (from: get_variables_all())
  #  - 70 > Comunidades y Ciudades Autónomas > CCAA
  #  - 115 > Provincias > PROV
  #  - 19 > Municipios > MUN
  geographical_variables <- c(115, 19, 70)

  # Find variable data
  variable_data <- NULL
  for (variable_id in serie_variables_id) {
    if (variable_id %in% geographical_variables) {
      variables <- get_variables_all()
      variable_data <- variables[variables$Id == variable_id,]
    }
  }

  if (is.null(variable_data)) {
    stop(paste0("No geographical variables found "))
  }

  # Calculate last n serie data
  if (nult == 0) {
    nult <- get_serie_nult(serie)
  }

  # Get serie data
  serie_data <- get_data_serie(serie, det = 2, nult = nult)

  # Get natcode
  serie_natcode <- get_natcode(serie, variable_data)

  # Represent map and series
  highchart(type = "map") %>%
    hc_chart(backgroundColor = "#161C20", zoomType = "xy") %>%
    hc_add_series(mapData = map, showInLegend = FALSE, nullColor = "#424242", borderWidth = 0)

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
        # Check if column "Nombre" exists
        if ("Nombre" %in% names(series$Periodicidad)) {
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
