# API INE (Series)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


#' @title Get series
#' @description This function returns data or metadata from an operation, table or a serie
#' @param code (string) serie, operation or table identificator
#' @param resource (string) resource to access, by default \code{resource = "metadata"} to get serie metadata.
#'  Possible values are \code{metadata, operation, values, table, metadataoperation, data, by_granularity, by_common_parameters or nlast}
#' @param help (boolean) type any value for \code{resource} param and type \code{help = TRUE} to see params available for this \code{resource}.
#' @param ioe (boolean) \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param det (int) \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip (string) \code{tip = M} to obtain the metadata (crossing variables-values) of the series
#' @param lang (string) language used to obtain information
#' @param query (string) string separated by \code{AND} with syntax \code{variable = value} using natural language
#' @param p (int) periodicity, \code{p = 1} by default
#' @param date_start (string) start date in format \code{YYYY-MM-DD}
#' @param date_end (string) end date in format \code{YYYY-MM-DD}
#' @param nlast (int) last \code{n} serie values
#' @param classification (string) serie classification, if \code{classification = NULL} this value will be auto-detected
#' @param verbose (boolean) to show more information about this process, \code{verbose = FALSE} by default
#' @param benchmark (boolean) used to measure the performance of the system, \code{benchmark = FALSE} by default.
#' @param geographical_granularity (string) geographical granularity
#' @param temporal_granularity (string) temporal granularity
#' @examples
#' get_series("IPC206449")
#' get_series(resource = "metadata", help = TRUE)
#' get_series("IPC", resource = "operation")
#' get_series("IPC206449", resource = "values")
#' get_series(22350, resource = "table")
#' get_series("IPC", resource = "metadataoperation", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL")
#' get_series("IPC251541", resource = "nlast")
#' get_series("IPC206449", resource = "data", nlast = 5)
#' get_series("IPC", resource = "by_granularity", geographical_granularity = "CCAA", verbose = TRUE)
#' get_series("IPC251539", resource = "by_common_parameters", verbose = TRUE)
#' @export
get_series <- function(code = NULL, resource = "metadata", help = FALSE, ioe = FALSE, det = 0, tip = NULL, lang = "ES", query = NULL, p = NULL, date_start = NULL, date_end = NULL, nlast = NULL, classification = NULL, verbose = FALSE, benchmark = FALSE, geographical_granularity = NULL, temporal_granularity = NULL) {

  content <- NULL

  switch(resource,
    # Get serie metadata
    metadata = {
      # Help
      if (help) {
        params <- c("code (serie id)", "det", "tip", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series("IPC206449")'))
        message(paste0('Example (extended): get_series(code = "IPC206449", resource = "metadata", det = 2, tip = "M", lang = "ES")'))
      } else {
        content <- get_serie_metadata(code, det, tip, lang)
      }
    },
    # Get series of an operation from cache
    operation = {
      # Help
      if (help) {
        params <- c("code (operation id)", "ioe", "benchmark")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series("IPC", resource = "operation")'))
        message(paste0('Example (extended): get_series(code = "IPC", resource = "operation", ioe = FALSE, benchmark = FALSE)'))
      } else {
        content <- get_series_operation_cache(code, ioe, benchmark)
      }
    },
    # Get serie values
    values = {
      # Help
      if (help) {
        params <- c("code (serie id)", "det", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series("IPC206449", resource = "values")'))
        message(paste0('Example (extended): get_series(code = "IPC206449", resource = "values", det = 1, lang = "ES")'))
      } else {
        content <- get_series_values(code, det, lang)
      }
    },
    # Get serie tables
    table = {
      # Help
      if (help) {
        params <- c("code (table id)", "det", "tip", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series(22350, resource = "table")'))
        message(paste0('Example (extended): get_series(code = 22350, resource = "table", det = 2, tip = "M", lang = "ES")'))
      } else {
        content <- get_series_table(code, det, tip, lang)
      }
    },
    # Get series by metadata crossing
    metadataoperation = {
      # Help
      if (help) {
        params <- c("code (operation id)", "query", "p", "det", "tip", "ioe", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series("IPC", resource = "metadataoperation", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL")'))
        message(paste0('Example (extended): get_series(code = "IPC", resource = "metadataoperation", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL", p = 1, det = 2, tip = "M", ioe = FALSE, lang = "ES")'))
      } else {
        content <- get_series_metadataoperation(code, query, p, det, tip, ioe, lang)
      }
    },
    # Get serie data
    data = {
      # Help
      if (help) {
        params <- c("code (serie id)", "date_start", "date_end", "nlast", "det", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series("IPC206449", resource = "data", nlast = 5)'))
        message(paste0('Example (extended): get_series(code = "IPC206449", resource = "data", date_start = "2013-01-01", data_end = "2016-01-01", nlast = NULL, det = 2, lang = "ES")'))
      } else {
        content <- get_data_serie(code, date_start, date_end, nlast, det, lang)
      }
    },
    # Get series filtered by granularity (temporal or geographical granularity)
    by_granularity = {
      # Help
      if (help) {
        params <- c("code (operation id)", "geographical_granularity", "temporal_granularity", "verbose")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series("IPC", resource = "by_granularity", geographical_granularity = "CCAA", verbose = TRUE)'))
        message(paste0('Example (extended): get_series(code = "IPC", resource = "by_granularity", geographical_granularity = "CCAA", temporal_granularity = "Mensual", verbose = TRUE)'))
      } else {
        content <- get_series_by_granularity(code, geographical_granularity, temporal_granularity, verbose)
      }
    },
    # Get series filtered by common parameters (name, geographical granularity and classification)
    by_common_parameters = {
      # Help
      if (help) {
        params <- c("code (serie id)", "classification", "verbose")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series("IPC251539", resource = "by_common_parameters", verbose = TRUE)'))
        message(paste0('Example (extended): get_series(code = "IPC251539", resource = "by_common_parameters", classification = "Base 1992", verbose = TRUE)'))
      } else {
        content <- get_series_by_common_parameters(code, classification, verbose)
      }
    },
    # Get serie n-last data
    nlast = {
      # Help
      if (help) {
        params <- c("code (serie id)")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_series("IPC251541", resource = "nlast")'))
        message(paste0('Example (extended): get_series("IPC251541", resource = "nlast")'))
      } else {
        content <- get_serie_nlast(code)
      }
    },
    {
      stop('ERROR: Possible values of param "resource" are: metadata, operation, values, table, metadataoperation, data, by_granularity, by_common_parameters or nlast')
    }
  )

  if (!is.null(content)) {
    return(content)
  }

}


# Get serie metadata (Private)
# Old name: get_serie()
# Examples:
# get_serie_metadata("IPC206449")
# get_serie_metadata("IPC206449", det = 2, tip = "M")
get_serie_metadata <- function(serie, det = 0, tip = NULL, lang = "ES") {

  # Checking options
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")

  if ((tip != "M") && (!is.null(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")

  # URL definition
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE/", serie, "?det=", det, "&tip=", tip)

  # Get data
  data <- fromJSON(url)

  return(data)

}


# Get all series (metadata) of an operation from the cache (Private)
# Old name: get_series_operation(cache = TRUE)
# Examples:
# get_series_operation_cache(25)
# get_series_operation_cache(30138, ioe = TRUE)
# get_series_operation_cache("IPC")
get_series_operation_cache <- function(operation, ioe = FALSE, benchmark = FALSE) {

  # Start the clock!
  if (benchmark) {
    rnorm(100000)
    rep(NA, 100000)
    ptm <- proc.time()
  }

  # Convert code to ID
  operations <- get_operations_all()
  if (class(operation) == "character") {
    operation <- operations[operations$Codigo == operation,]$Id
  } else {
    if (ioe) {
      operation <- operations[operations$Cod_IOE == operation,]$Id
    }
  }

  # Get data from cache
  data <- get_cache_rds(operation, type = "SERIEOPERATION")
  if (is.null(data)) {
    return(NULL)
  }

  # Stop the clock
  if (benchmark) {
    print(proc.time() - ptm)
  }

  return(data)

}


# Get all series (metadata) of an operation from API (Private)
# Old name: get_series_operation(cache = FALSE)
# Examples:
# get_series_operation_api(25, pagination = FALSE, page = 1)
# get_series_operation_api(25, pagination = TRUE, page_start = 1, page_end = 2)
get_series_operation_api <- function(operation, det = 0, tip = NULL, pagination = FALSE, page = NULL, page_start = NULL, page_end = NULL, ioe = FALSE, lang = "ES", benchmark = FALSE) {

  # Checking options
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")

  if ((tip != "M") && (!is.null(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")

  # Start the clock!
  if (benchmark) {
    rnorm(100000)
    rep(NA, 100000)
    ptm <- proc.time()
  }

  # Convert code to ID
  operations <- get_operations_all()
  if (class(code) == "character") {
    operation <- operations[operations$Codigo == operation,]$Id
  } else {
    if (ioe) {
      operation <- operations[operations$Cod_IOE == operation,]$Id
    }
  }

  # Get data from API
  data <- NULL

  if (pagination) {

    empty_content <- FALSE
    page <- 1

    if (!is.null(page_start)) {
      if (page_start <= 0) {
        stop("You have defined 'page_start' parameter with an incorrect value.")
      } else {
        page <- page_start
      }
    }

    while (!empty_content) {

      # URL definition
      if (ioe) {
        url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/IOE", operation, "?page=", page, "&det=", det, "&tip=", tip)
      }
      else {
        url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", operation, "?page=", page, "&det=", det, "&tip=", tip)
      }

      #print(url)
      # Get content
      content <- get_content(url, max_iterations = 3, seconds = 30)

      if (length(content) == 0) {
        empty_content <- TRUE
        next
        # print(paste0("No content found in page ", page))
      } else {
        data_content <- NULL
        for (i in 1:nrow(content)) {
          # Id
          if ((tip == "M") && (det == 2)) {
            data_content$Id <- rbind(data_content$Id, content$Id[i])
            data_content$Operacion <- rbind(data_content$Operacion, content$Operacion$Id[i])
          }
          data_content$COD <- rbind(data_content$COD, content$COD[i])
          data_content$T3_Operacion <- rbind(data_content$T3_Operacion, content$T3_Operacion[i])
          data_content$Nombre <- rbind(data_content$Nombre, content$Nombre[i])
          data_content$Decimales <- rbind(data_content$Decimales, content$Decimales[i])
          data_content$T3_Periodicidad <- rbind(data_content$T3_Periodicidad, content$T3_Periodicidad[i])
          data_content$T3_Publicacion <- rbind(data_content$T3_Publicacion, content$T3_Publicacion[i])
          data_content$T3_Clasificacion <- rbind(data_content$T3_Clasificacion, content$T3_Clasificacion[i])
          data_content$T3_Escala <- rbind(data_content$T3_Escala, content$T3_Escala[i])
          data_content$T3_Unidad <- rbind(data_content$T3_Unidad, content$T3_Unidad[i])
          # Periodicidad (nomrbe) y Metadata
          if ((tip == "M") && (det == 2)) {
            # Check if classification is null
            if (is.null(content$Clasificacion$Nombre[i])) {
              data_content$Clasificacion <- rbind(data_content$Clasificacion, NA)
            } else {
              data_content$Clasificacion <- rbind(data_content$Clasificacion, content$Clasificacion$Nombre[i])
            }
            data_content$Unidad <- rbind(data_content$Unidad, content$Unidad$Nombre[i])
            data_content$Periodicidad <- rbind(data_content$Periodicidad, content$Periodicidad$Nombre[i])
            data_content$MetaData <- rbind(data_content$MetaData, content$MetaData[i])
          }

        }

      }

      # Convert to data frame
      data_content <- data.frame(data_content, stringsAsFactors = FALSE)

      # Build data content
      data <- rbind(data, data_content)

      if (!is.null(page_end)) {
        if (page == page_end)
          break
      }

      page <- page + 1

    }

    # Convert to data frame
    data <- data.frame(data, stringsAsFactors = FALSE)

    # Get all data
  } else {

    if (is.null(page)) {
      # URL definition
      if (ioe) {
        url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/IOE", code, "?page=", NULL, "&det=", det, "&tip=", tip)
      }
      else {
        url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", code, "?page=", NULL, "&det=", det, "&tip=", tip)
      }
    } else {
      # URL definition
      if (ioe) {
        url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/IOE", code, "?page=", page, "&det=", det, "&tip=", tip)
      }
      else {
        url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", code, "?page=", page, "&det=", det, "&tip=", tip)
      }
    }

    data <- fromJSON(url)
  }

  save_to_rds(data, code, type = "SERIEOPERATION")



  # Stop the clock
  if (benchmark) {
    print(proc.time() - ptm)
  }

  return(data)

}


# Get series values (Private)
# Old name: get_series_values()
# get_series_values("IPC206449")
# get_series_values("IPC206449", det = 1)
get_series_values <- function(code, det = 0, lang = "ES") {

  # Check params
  if ((det < 0) || (det > 1)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_SERIE/", code, "?det=", det)

  # Get content
  content <- fromJSON(url)

  return(content)

}


# Get series table (Private)
# Old name: get_series_table()
# Examples:
# get_series_table(22350)
# get_series_table(22350, 2, "M")
get_series_table <- function(code, det = 0, tip = NULL, lang = "ES") {

  # Check det param
  if ((det < 0) || (det > 2)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  # Check tip param
  if ((tip != "M") && (!is.null(tip))) {
    stop("You have defined 'tip' parameter with an incorrect value.")
  }

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_TABLA/", code, "?det=", det, "&tip=", tip)

  # Get content
  content <- fromJSON(url)

  return(content)
}


# Get series metadata operation (Private)
# Old name: get_series_metadataoperation()
# Examples:
# get_series_metadataoperation("IPC", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL")
get_series_metadataoperation <- function(code, query = NULL, p = NULL, det = 0, tip = NULL, ioe = FALSE, lang = "ES") {

  # Checking params
  if ((det < 0) || (det > 2)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  if ((p <= 0) && (!is.null(p))) {
    stop("You have defined 'p' (periodicity) parameter with an incorrect value.")
  }

  if ((tip != "M") && (!is.null(tip))) {
    stop("You have defined 'tip' parameter with an incorrect value.")
  }

  # Split query
  df_queries <- NULL
  queries <- strsplit(query, split = "AND")
  for (queries_splited in queries) {
    var_val <- strsplit(queries_splited, split = "=")
    for (string in var_val) {
      df_queries <- rbind(df_queries, data.frame(string))
    }
  }

  # Get variables and values id
  variables <- get_variables_operation(code)
  result <- NULL
  count <- 0
  g <- 1
  for (qvalue in df_queries$string) {
    qvalue <- trimws(as.character(qvalue))
    if (count %% 2 == 0) { # variables
      variableId <- variables[match(qvalue, variables[["Nombre"]]),][["Id"]]
      variable <- paste0("g", g, "=", variableId, ":")
      result <- rbind(result, variable)
      g <- g + 1
    # values
    } else {
      value <- get_values_variableoperation(variableId, code)
      value <- value[match(qvalue, value[["Nombre"]]),][["Id"]]
      if (is.na(value)) {
        value <- ""
      }
      value <- paste0(value, "&")
      result <- rbind(result, value)
    }
    count <- count + 1
  }

  # Join query
  urlStr <- paste0(result, collapse = "")

  # Build URL
  if (ioe) {
    if (is.null(p)) {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/IOE", code, "?", urlStr, "tip=", tip, "&det=", det)
    } else {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/IOE", code, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det)
    }
  } else {
    if (is.null(p)) {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/", code, "?", urlStr, "tip=", tip, "&det=", det)
    } else {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/", code, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det)
    }
  }

  return(fromJSON(url))

}


# Get serie last-n (Private)
# Old name: get_serie_nult()
# Example:
# get_serie_nlast("IPC251541")
get_serie_nlast <- function(serie) {

  # Last "n"
  nult <- 0

  # Get metadata serie
  serie_metadata <- get_series(serie, det = 2, tip = "M")

  periodicity <- serie_metadata$Periodicidad$Nombre
  if (periodicity == "Mensual") {
    nult <- 12
  } else {
    if (periodicity == "Anual") {
      nult <- 1
    } else {
      nult <- 10
    }
  }

  return(nult)

}
