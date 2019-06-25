# API INE (Polygons)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


#' @title Get geographical
#' @description This function returns geographical information
#' @param code (string) serie id
#' @param resource (string) resource to access, by default \code{resource = "variable"} to get serie metadata.
#'  Possible values are \code{variable, natcode or natcode_to_geocode}
#' @param all (boolean) if \code{all = TRUE} you will get all natcodes
#' @param verbose (boolean) show more information during the process
#' @param natcode (int) geographical code from INE
#' @param geocode (string) geographical code from Eurostat
#' @param exponential_notation (boolean) to show or not exponential notation. \code{e.g. e+10}
#' @param help (boolean) type any value for \code{resource} param and type \code{help = TRUE} to see params available for this \code{resource}.
#' @examples
#' get_geographical("IPC251522")
#' get_geographical(resource = "variable", help = TRUE)
#' get_geographical("IPC251522", resource = "natcode")
#' get_geographical(natcode = 34050000000, resource = "natcode_to_geocode")
#' @export
get_geographical <- function(code = NULL, resource = "variable", all = FALSE, verbose = FALSE, natcode = NULL, geocode = NULL, exponential_notation = FALSE, help = FALSE) {

  content <- NULL

  switch(resource,
    variable = {
      # Help
      if (help) {
        params <- c("code (serie id)")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_geographical("IPC251522")'))
        message(paste0('Example (extended): get_geographical("IPC251522", resource = "variable")'))
      } else {
        content <- get_geographical_variable(code)
      }
    },
    natcode = {
      # Help
      if (help) {
        params <- c("code (serie id)", "all", "verbose")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_geographical("IPC251522", resource = "natcode")'))
        message(paste0('Example (extended): get_geographical("IPC251522", resource = "natcode", all = FALSE, verbose = FALSE)'))
      } else {
        content <- get_natcode(code, all, verbose)
      }
    },
    natcode_to_geocode = {
      # Help
      if (help) {
        params <- c("natcode", "geocode", "exponential_notation")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_geographical(natcode = 34050000000, resource = "natcode_to_geocode")'))
        message(paste0('Example (extended): get_geographical(natcode = 34050000000, geocode = NULL, resource = "natcode_to_geocode", exponential_notation = FALSE)'))
      } else {
        content <- convert_natcode_to_geocode(natcode, geocode, exponential_notation)
      }
    },
    {
      stop('ERROR: Possible values of param "resource" are: variable, natcode or natcode_to_geocode')
    }
  )

  if (!help) {
    return(content)
  }

}


# Get geographical variable
# How to call: get_geographical("IPC251522", resource = "variable")
# Examples:
# get_geographical_variable("IPC251522")
get_geographical_variable <- function(serie) {

  # Variables
  variable_data <- NULL

  # Get serie metadata
  serie_metadata <- get_series(serie, det = 2, tip = "M")

  # Get serie variables (id)
  serie_variables_id <- serie_metadata$MetaData$Variable$Id

  # Variables (from: get_variables_all())
  #  - 70 > Comunidades y Ciudades Autónomas > CCAA
  #  - 115 > Provincias > PROV
  #  - 19 > Municipios > MUN
  geographical_variables <- c(115, 19, 70)

  # Find variable data
  for (variable_id in serie_variables_id) {
    if (variable_id %in% geographical_variables) {
      variables <- get_variables_all()
      variable_data <- variables[variables$Id == variable_id,]
    }
  }

  return(variable_data$Id)
}

# Get natcode
# How to call: get_geographical("IPC251522", resource = "natcode")
# Examples:
# get_natcode()
# get_natcode("IPC251522")
# get_natcode("IPC251541")
# get_natcode("DPOP37286")
get_natcode <- function(serie = NULL, all = TRUE, verbose = TRUE) {

  # Natcode format:
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
  natcode_list <- get_cache_rds("natcodes", type = "DATATABLE")
  natcode_df <- data.frame(natcode_list)

  # Get all natcode
  if (is.null(serie)) {
    if (all) {
      return(natcode_df)
    }
  }

  # Get serie metadata
  serie_metadata <- get_series(serie, det = 2, tip = "M")

  # Get geographical variable id
  variable_id <- get_geographical_variable(serie)

  # Get geographical variable code
  variable_codigo <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Id == variable_id,]$Variable$Codigo

  if (length(variable_codigo) == 0) {
    variables <- get_variables_all()
    if (verbose) {
      message(paste0("WARNING: maybe serie ", serie, ", has not this geograhpical granularity (", variables[variables$Id == variable_id,]$Nombre, ")"))
    }

    natcode <- paste0(natcode, "00", "00", "00", "000")
    return(natcode)

  }

  variable_id <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Id == variable_id,]$Codigo

  # Some variables has no code, for example: serie DPOP36914
  if ((variable_id == "") || (length(variable_id) == 0)){
    variable_id <- "00000"
  }

  # Provincias
  if (variable_codigo == "PROV") {
    lista_ccaa <- natcode_df[natcode_df$CPRO == as.numeric(variable_id),]$CODAUTO
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
      # If there is not code for this MUN
      if (length(cod_ccaa) == 0) {
        natcode <- paste0(natcode, "00", "00", "00", "000")
        return(natcode)
      } else {
        if (nchar(trunc(cod_ccaa)) == 1) {
          cod_ccaa <- paste0("0", cod_ccaa)
        }
      }


      natcode <- paste0(natcode, cod_ccaa, cod_prov, cod_prov, cod_mun)

    }
  }

  return(natcode)

}


# Convert Natcode to Geocode
# How to call: get_geographical(natcode = 34050000000, resource = "natcode_to_geocode")
# Examples:
# convert_natcode_to_geocode()
# convert_natcode_to_geocode(natcode = 34050000000)
# convert_natcode_to_geocode(geocode = "ES70")
convert_natcode_to_geocode <- function(natcode = NULL, geocode = NULL, exponential_notation = FALSE) {

  # Force R not to use exponential notation (e.g. e+10)
  # Source: https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html
  if (!exponential_notation) {
    options("scipen" = 100, "digits" = 4)
  }

  # Get the complete table used for codes conversion.
  data <- get_cache_rds("natcode_to_geocode", type = "DATATABLE")
  data <- data.frame(data, stringsAsFactors = FALSE)
  code <- NULL

  # If all params are null, you will get the complete table used for codes conversion.
  if ((is.null(natcode)) && (is.null(geocode))) {
    return(data)
  }

  # Convert natcode to/from geocode
  if ((!is.null(natcode)) && (is.null(geocode))) {
      code <- data[data$natcode == natcode,]$geocode
      code <- as.character(code)
  } else {
    if ((is.null(natcode)) && (!is.null(geocode))) {
      code <- data[data$geocode == geocode,]$natcode
    }
  }

  # If no results, return NULL
  if (length(code) == 0) {
    code <- NULL
  }

  # Check if code not found
  if (is.null(code)) {
    if ((!is.null(natcode)) && (is.null(geocode))) {
      message(paste0("Error: no code found for natcode ", natcode))
    } else {
      if ((is.null(natcode)) && (!is.null(geocode))) {
        message(paste0("Error: no code found for geocode ", geocode))
      }
    }
  }

  return(code)
}


#' @title Draw serie
#' @description This function allows representing series data into a map
#' @param serie (string) serie id
#' @param nult (int) last \code{n} serie data, if \code{nult = 0} this value will be auto-calculated
#' @param classification (string) serie classification, if \code{classification = NULL} this value will be auto-detected
#' @param map_scale (int) refers to the relationship or ratio between distance on a map and the corresponding distance on the ground.
#' For example, on a \code{1:1000000} scale map, 1cm on the map equals 1km on the ground. Possible values are: \code{1, 3, 10, 20 or 60}, and
#' it's only for PROV or CCAA geographical granularity, \code{map_scale = 60} by default and \code{map_scale = NULL} for high detailed map.
#' @param verbose (boolean) show more information during the process
#' @param benchmark (boolean) used to measure the performance of the system, \code{benchmark = FALSE} by default.
#' @examples
#' draw_serie("IPC251521")
#' draw_serie("IPC251541")
#' draw_serie("UA42121")
#' @export
draw_serie <- function(serie, nult = 0, classification = NULL, map_scale = 60, verbose = FALSE, benchmark = FALSE) {

  # Start the clock (data)
  if (benchmark) {
    rnorm(100000)
    rep(NA, 100000)
    ptm <- proc.time()
  }

  # Variables
  geographical_granularity <- NULL
  serie_name <- NULL
  data <- NULL

  # Message
  message("Note: represent all polygons may take much time, please be patient ...")

  # Get all related series from a serie
  series <- get_series_by_common_parameters(serie, classification = classification, verbose = verbose)

  # DATA
  message("Getting data ...")

  # Get data from all series
  for (i in 1:length(series)) {

    # Calculate last n serie data
    if (nult == 0) {
      nult <- get_series(series[i], resource = "nlast")
    }

    # Get serie data
    #serie_data <- get_data_serie(serie, det = 2, nult = nult)
    serie_data <- get_data_serie(series[i], det = 2, nult = 1)

    # Get natcode
    serie_natcode <- get_natcode(series[i], verbose = verbose)

    # Generate dataframe with necesary data
    data$name <- rbind(data$name, series[i])
    data$value <- rbind(data$value, serie_data$Data$Valor)
    if (is.null(map_scale)) {
      data$natcode <- rbind(data$natcode, serie_natcode)
    } else {
      data$geocode <- rbind(data$geocode, convert_natcode_to_geocode(natcode = serie_natcode))
    }

  }

  # Conert to data frame
  data <- data.frame(data, stringsAsFactors = FALSE)

  # Stop the clock (data)
  if (benchmark) {
    time <- (proc.time() - ptm)[[3]]
    message(paste0("Time elapsed (data): ", time, " (s)"))
  }

  # Start the clock (polygons)
  if (benchmark) {
    rnorm(100000)
    rep(NA, 100000)
    ptm <- proc.time()
  }

  # POLYGONS
  message("Building polygons ...")

  # Get serie metadata
  serie_metadata <- get_series(serie, det = 2, tip = "M")
  # Get geographical variable id
  variable_id <- get_geographical_variable(serie)
  # Get geographical variable code
  variable_codigo <- serie_metadata$MetaData[serie_metadata$MetaData$Variable$Id == variable_id,]$Variable$Codigo

  if (variable_codigo == "PROV") {
    geographical_granularity <- "provincias"
  } else {
    if (variable_codigo == "MUN") {
      geographical_granularity <- "municipios"
      if (!is.null(map_scale)) {
        stop(paste0("Error: geographical granularity Municipios (MUN) has no map scaled, please use map_scale = NULL instead of map_scale = ", map_scale, "."))
      }
    } else {
      geographical_granularity <- "comunidades_autonomas"
    }
  }


  # Get serie name
  name_splited <- strsplit(serie_metadata$Nombre, "[.] ")[[1]]
  for (i in 1:length(name_splited)) {
    if (i > 1) {
      serie_name <- paste0(serie_name, name_splited[i], ". ")
    }
  }

  operation_name <- serie_metadata$Operacion$Nombre

  # Get polygon from cache
  joinby <- NULL
  if (is.null(map_scale)) {
    map <- get_cache_rds(geographical_granularity, type = "POLYGONS")
    joinby <- "natcode"
    properties_name <- "{point.properties.nameunit}"
  } else {
    object <- paste0(geographical_granularity, "-EUROSTAT-", map_scale)
    map <- get_cache_rds(object, type = "POLYGONS")
    joinby <- "geocode"
    properties_name <- "{point.properties.name}"
  }


  # Represent map and series
  hc <- highchart(type = "map") %>%
    hc_chart(backgroundColor = "#ffffff", zoomType = "xy") %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_colorAxis(min = min(data$value), max = max(data$value), type = 'logarithmic') %>%
    hc_title(text = operation_name) %>%
    hc_subtitle(text = serie_name) %>%
    hc_add_series(
      mapData = map,
      data = data,
      showInLegend = FALSE,
      borderWidth = 0,
      keys = c("name", "value", joinby),
      name = serie_metadata$MetaData[serie_metadata$MetaData$Variable$Id == variable_id,]$Variable$Nombre,
      joinBy = joinby,
      dataLabels = list(enabled = TRUE, format = properties_name),
      tooltip = list(pointFormat = paste0(properties_name, ": <b>{point.value}</b> (", serie_metadata$Unidad$Nombre, ")"))
    )

  # Stop the clock (polygons)
  if (benchmark) {
    time <- (proc.time() - ptm)[[3]]
    message(paste0("Time elapsed (polygons): ", time, " (s)"))
  }

  return(hc)

}


# Get operations by granularity
# Examples:
# get_operations_by_granularity(geographical_granularity = "PROV")
# get_operations_by_granularity(temporal_granularity = "Anual")
# get_operations_by_granularity(geographical_granularity = "PROV", temporal_granularity = "Mensual")
get_operations_by_granularity <- function(geographical_granularity = NULL, temporal_granularity = NULL, verbose = TRUE) {

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
      if (verbose) {
        print(paste0("Seraching in operation: ", operation))
      }

      # Geographical and temporal granularity
      if ((!is.null(geographical_granularity)) && (!is.null(temporal_granularity))) {

        series <- get_series_operation(operation)
        variables <- get_variables_operation(operation)

        if ("Nombre" %in% names(series)) {
          if (!is.null(variables$Codigo)) {
            if ((temporal_granularity %in% series$Periodicidad) && (geographical_granularity %in% variables$Codigo)) {
              operations <- c(operations, operation)
              if (verbose) {
                print(paste0("Found (", geographical_granularity, " and ", temporal_granularity, ") in operation: ", operation))
              }
              next
            }
          }
        }


      # Temporal granularity
      } else if ((is.null(geographical_granularity)) && (!is.null(temporal_granularity))) {
        series <- get_series_operation(operation)
        # Check if column "Nombre" exists
        if ("Nombre" %in% names(series)) {
          if (temporal_granularity %in% series$Periodicidad) {
            operations <- c(operations, operation)
            if (verbose) {
              print(paste0("Found (", temporal_granularity, ") in operation: ", operation))
            }
            next
          }
        }

      # Geographical granularity
      } else if ((!is.null(geographical_granularity)) && (is.null(temporal_granularity))) {
        variables <- get_variables_operation(operation)
        if (!is.null(variables$Codigo)) {
          if (geographical_granularity %in% variables$Codigo) {
            operations <- c(operations, operation)
            if (verbose) {
              print(paste0("Found (", geographical_granularity, ") in operation: ", operation))
            }
            next
          }
        }
      }
    }
  }

  return(operations)

}

# Get series by granularity (Private)
# Examples
# get_series_by_granularity("IPC", geographical_granularity = "PROV")
# get_series_by_granularity("IPC", temporal_granularity = "Mensual")
# get_series_by_granularity("IPC", geographical_granularity = "PROV", temporal_granularity = "Mensual")
get_series_by_granularity <- function(operation, geographical_granularity = NULL, temporal_granularity = NULL, verbose = TRUE) {

  # Get series list
  series <- get_series(operation, resource = "operation")
  series_list <- c()

  for (serie in series$COD) {

    # Get serie metadata
    serie_metadata <- get_series(serie, det = 2, tip = "M")

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


# Get series by common parameters (Private)
# Examples:
# get_series_by_common_parameters("IPC251539", classification = "Base 1992")
# get_series_by_common_parameters("IPC251539")
# get_series_by_common_parameters("DPOP37286")
# get_series_by_common_parameters("UA42121")
get_series_by_common_parameters <- function(serie, classification = NULL, verbose = TRUE) {


  # Get serie metadata
  serie_metadata <- get_series(serie, det = 2, tip = "M")

  # Get classification name
  name_splited <- strsplit(serie_metadata$Nombre, "[.] ")[[1]]
  name <- NULL
  for (i in 1:length(name_splited)) {
    if (i > 1) {
      name <- paste0(name, name_splited[i], ". ")
    }

  }

  message(paste0("Getting series for '", name, "' ..."))

  # Parse name
  name <- parse_param(name)

  # Get operation
  operacion <- serie_metadata$Operacion$Id

  # Variable ID
  geographical_variables <- c(115, 19, 70)
  variable_id <- serie_metadata$MetaData$Variable$Id
  geographical_id <- 0
  for (variable in variable_id) {
    if (variable %in% geographical_variables) {
      geographical_id <- variable
    }
  }

  # Get geographical varaibles
  geographical_name <- serie_metadata$MetaData$Variable[serie_metadata$MetaData$Variable$Id == geographical_id,]$Nombre
  geographical_code <- serie_metadata$MetaData$Variable[serie_metadata$MetaData$Variable$Id == geographical_id,]$Codigo
  message(paste0("Note: serie ", serie," has '", geographical_name, " (", geographical_code, ")' as geographical granularity."))

  # Get series operation
  series <- get_series(operacion, resource = "operation")

  # Autoselect classification if is NULL
  if (is.null(classification)) {
    # If there is more than one classification for this serie
    if (nrow(series[series$Nombre == serie_metadata$Nombre,]) > 1) {
      list_classification <- series[series$Nombre == serie_metadata$Nombre,]
      # Get the last classification
      classification <- list_classification[nrow(list_classification),]$Clasificacion
      message(paste0("Note: we've found more of one classification in serie ", serie, ", but we've selected the last classification (", classification, ") for you"))
    } else {
      # If there is only one classification for this serie
      classification <- series[series$Nombre == serie_metadata$Nombre,]
    }
  }

  series_list <- c()
  variables <- get_variables_all()

  # Get series
  for (i in 1:nrow(series)) {

    if (grepl(pattern = name, x = series$Nombre[i])) {

      # Nota: si da problemas -> revisar MetaData o Metadata
      serie_variables_id <- series$MetaData[[i]]$Variable$Id

      # Variables (from: get_variables_all())
      #  - 70 > Comunidades y Ciudades Autónomas > CCAA
      #  - 115 > Provincias > PROV
      #  - 19 > Municipios > MUN
      geographical_variables <- c(115, 19, 70)

      # Find variable data
      variable_data <- NULL
      for (variable_id in serie_variables_id) {
        if (variable_id %in% geographical_variables) {
          variable_data <- variables[variables$Id == variable_id,]
        }
      }

      variable_id <- variable_data$Id

      # No classification found
      if (is.null(series$Clasificacion[i])) {
        if (variable_id == geographical_id) {
          series_list <- c(series_list, series$COD[i])
          if (verbose) {
            print(paste0("Found (", series$COD[i], "): ", series$Nombre[i]))
          }
        }

      # Classification found
      } else {
        if (!is.null(variable_id)) {

          # If there is not classification
          if ((is.na(series$Clasificacion[i])) || (is.na(classification))) {
            if (variable_id == geographical_id) {
              series_list <- c(series_list, series$COD[i])
              if (verbose) {
                print(paste0("Found (", series$COD[i], "): ", series$Nombre[i]))
              }
            }

          # If there is classification
          } else {
            if ((variable_id == geographical_id) && (series$Clasificacion[i] == classification)) {
              series_list <- c(series_list, series$COD[i])
              if (verbose) {
                print(paste0("Found (", series$COD[i], "): ", series$Nombre[i]))
              }
            }
          }

        }
        #else {
        #  print(paste0("No se han encontrado coincidencias geográficas en esta serie"))
        #}

      }

    }
  }

  return(series_list)

}
