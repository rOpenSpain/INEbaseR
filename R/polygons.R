# API INE (Polygons)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get natcode
#' @description This function allows get all natcodes or calculate a natcode from a serie and a geographical granularity
#' @param serie (string) serie identificator
#' @param variable_id (int) variable identificator
#' @param all (bool) if \code{all = TRUE} you will get all natcodes
#' @examples
#' get_natcode()
#' get_natcode("IPC251541", 115)
#' get_natcode("DPOP37286", 19)
#' get_natcode("IPC251522", 70)
#' @export
get_natcode <- function(serie = NULL, variable_id = NULL, all = TRUE) {

  # Checking errors
  if ((all) && (!is.null(serie)) && (is.null(variable_id))) {
    stop("ERROR: parameter variable_id is NULL.")
  }

  if ((all) && (!is.null(variable_id)) && (is.null(serie))) {
    stop("ERROR: parameter serie is NULL.")
  }

  geographical_variables <- c(115, 19, 70)

  if (!is.null(variable_id)) {
    if (!variable_id %in% geographical_variables) {
      stop(paste0("ERROR: variable_id is ", variable_id, ", and should be one of this: 115, 19, 70"))
    }
  }

  # Natcode format (map):
  # (2 digits): 34 (Spain)
  # CAUTO:
  # (2 digits): CAUTO
  # CPRO:
  # (2 digits): CPRO
  # MUN:
  # (2 digits): CPRO
  # (3 digits): CMUN

  # Natcode
  natcode <- "34" # Spain prefix: 34

  # Get natcode table from cache
  natcode_list <- get_rds_content("natcodes", type = "/DATATABLE-")
  natcode_df <- data.frame(natcode_list)

  # Get all natcode
  if ((is.null(serie)) &&  (is.null(variable_id))) {
    if (all) {
      return(natcode_df)
    } else {
      stop(paste0("ERROR: Please, use all = TRUE option."))
    }
  }

  # Get serie metadata
  serie_metadata <- get_serie(serie, det = 2, tip = "M")

  variable_codigo <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Id == variable_id,]$Variable$Codigo

  if (length(variable_codigo) == 0) {
    variables <- get_variables_all()
    stop(paste0("ERROR: maybe serie ", serie, ", has not this geograhpical granularity (", variables[variables$Id == variable_id,]$Nombre, ")"))
  }

  variable_id <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Id == variable_id,]$Codigo

  # Provincias
  if (variable_codigo == "PROV") {
    lista_ccaa <- natcode_df[natcode_df$CPRO == variable_id,]$CODAUTO
    cod_ccaa <- unique(lista_ccaa)

    if (length(cod_ccaa) > 1) {
      cod_ccaa <- cod_ccaa[1]
    }

    # Adding "0" to fill up blank digits
    if (nchar(trunc(cod_ccaa)) == 1) {
      cod_ccaa <- paste0("0", cod_ccaa)
    }

    natcode <- paste0(natcode, cod_ccaa, variable_id, "00", "000")

  } else {
    # Comunidades autónomas
    if (variable_codigo == "CCAA") {
      cod_ccaa <- variable_id
      natcode <- paste0(natcode, cod_ccaa, "00", "00", "000")
    # Municipios
    } else {

      cod_prov <- strsplit(as.character(variable_id), "")[[1]][1:2]
      cod_prov <- paste(cod_prov, collapse = '')

      cod_mun <- strsplit(as.character(variable_id), "")[[1]][3:5]
      cod_mun <- paste(cod_mun, collapse = '')

      lista_ccaa <- natcode_df[natcode_df$CPRO == as.numeric(cod_prov) & natcode_df$CMUN == as.numeric(cod_mun),]$CODAUTO
      cod_ccaa <- unique(lista_ccaa)

      if (length(cod_ccaa) > 1) {
        cod_ccaa <- cod_ccaa[1]
      }

      # Adding "0" to fill up blank digits
      if (nchar(trunc(cod_ccaa)) == 1) {
        cod_ccaa <- paste0("0", cod_ccaa)
      }

      natcode <- paste0(natcode, cod_ccaa, cod_prov, cod_prov, cod_mun)

    }
  }

  return(natcode)

}

#' @title Draw serie
#' @description This function allows representing series data into a map
#' @param serie (string) serie identificator
#' @param geographical_granularity (string) can be one of this: \code{comunidades_autonomas}, \code{provincias} or \code{municipios}
#' @param nult (int) last \code{n} serie data, if \code{nult = 0} this value will be auto-calculated
#' @examples
#' draw_serie("IPC251541", "provincias")
#' @export
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
  serie_natcode <- get_natcode(serie, variable_data$Id)

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

#' @title Get series by granularity
#' @description This function returns a list of all series of an operation that has a temporal or geographic granularity specified by the user
#' @param operation (string) operation identification
#' @param geographical_granularity (string) geographical granularity
#' @param temporal_granularity (string) temporal granularity
#' @param verbose (boolean) show more information during the process
#' @examples
#' get_series_by_granularity("IPC", geographical_granularity = "PROV")
#' get_series_by_granularity("IPC", temporal_granularity = "Mensual")
#' get_series_by_granularity("IPC", geographical_granularity = "PROV", temporal_granularity = "Mensual")
#' @export
get_series_by_granularity <- function(operation, geographical_granularity = NULL, temporal_granularity = NULL, verbose = TRUE) {

  # Get series list
  series <- get_series_operation(operation)
  series_list <- c()

  for (serie in series$COD) {

    # Get serie metadata
    serie_metadata <- get_serie(serie, det = 2, tip = "M")

    # Temporal and geographical granularity
    if ((!is.null(geographical_granularity)) && (!is.null(temporal_granularity))) {

      # Get geographical variable code: PROV, MUN, CCAA
      code <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Codigo == geographical_granularity,]$Variable$Codigo

      # Get temporal variable: Mensual, Anual ...
      periodicity <- serie_metadata$Periodicidad$Nombre

      if (length(code) > 0) {
        if ((code == geographical_granularity) && (periodicity == temporal_granularity)) {
          series_list <- c(series_list, serie)
          if (verbose) {
            print(paste0("Found (", geographical_granularity, " and ", temporal_granularity, ") in serie: ", serie, " > ", series[series$COD == serie,]$Nombre))
          }
        }
      }

    } else {

      # Geographical granularity
      if (!is.null(geographical_granularity)) {

        # Get geographical variable code: PROV, MUN, CCAA
        code <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Codigo == geographical_granularity,]$Variable$Codigo

        if (length(code) > 0) {
          if (code == geographical_granularity) {
            series_list <- c(series_list, serie)
            if (verbose) {
              print(paste0("Found (", geographical_granularity, ") in serie: ", serie, " > ", series[series$COD == serie,]$Nombre))
            }
          }
        }

      } else {
        # Temporal granularity
        if (!is.null(temporal_granularity)) {
          # Get temporal variable: Mensual, Anual ...
          periodicity <- serie_metadata$Periodicidad$Nombre
          if (periodicity == temporal_granularity) {
            series_list <- c(series_list, serie)
            if (verbose) {
              print(paste0("Found (", temporal_granularity, ") in serie: ", serie, " > ", series[series$COD == serie,]$Nombre))
            }
          }
        }
      }

    }

  }

  return(series_list)

}
