# API INE (Data)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


# Get data serie
# Examples:
# get_data_serie("IPC206449", nult = 1)
# get_data_serie("IPC206449", nult = 5)
# get_data_serie("IPC206449", "2013-01-01", "2016-01-01")
# get_data_serie("IPC206449", "2010-01-01")
get_data_serie <- function(serie, date_start = NULL, date_end = NULL, nult = 0, det = 0, lang = "ES") {

  # Checking options
  if (!is.null(date_end)) {
    if (date_start > date_end) {
      stop("Start date cannot be after the end date.")
    }
  }

  # Date format
  if (!is.null(date_start)) {
    date_start <- format.Date(date_start,'%Y%m%d')
  }

  if (!is.null(date_end)) {
    date_end <- format.Date(date_end,'%Y%m%d')
  }

  # Build URL
  if (nult == 0) {
    if (is.null(date_end)) {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_SERIE/", serie, "?date=", date_start, ":&det=", det)
    } else {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_SERIE/", serie, "?date=", date_start, ":", date_end, "&det=", det)
    }
  } else {
    if (is.null(date_end)) {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_SERIE/", serie, "?nult=", nult, "&date=", date_start, ":&det=", det)
    } else {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_SERIE/", serie, "?nult=", nult, "&date=", date_start, ":", date_end, "&det=", det)
    }
  }

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}


# Get data table
# Examples:
# get_data_table(22350, 4)
get_data_table <- function(id, nlast = 0, det = 0, tip = NULL, lang = "ES") {

  # Checking det
  if ((det < 0) || (det > 2)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }
  # Checking tip
  if ((tip != "AM") && (!is.null(tip))) {
    stop("You have defined 'tip' parameter with an incorrect value.")
  }

  # Build URL
  if (nlast == 0) {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_TABLA/", id)
  } else {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_TABLA/", id, "?nult=", nlast)
  }

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}


# Get data metadata-operation
# How to call: get_metadata_crossing("IPC", resource = "data", query = "Provincias = Madrid AND Tipo de dato = Variacion mensual AND Grupos ECOICOP = NULL", nlast = 5)
# Examples:
# get_data_metadataoperation("IPC", query = "Provincias = Madrid AND Tipo de dato = Variacion mensual AND Grupos ECOICOP = NULL", nult = 5)
get_data_metadataoperation <- function(operation, query = NULL, p = 1, nult = 1, det = 0, tip = NULL, lang = "ES") {

  # Checking det
  if ((det < 0) || (det > 2)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }
  # Checking p
  if (p < 0) {
    stop("You have defined 'p' (periodicity) parameter with an incorrect value.")
  }
  # Checking tip
  if ((tip != "AM") && (!is.null(tip))) {
    stop("You have defined 'tip' parameter with an incorrect value.")
  }
  # Checking nult
  if (nult < 1) {
    stop("You have defined 'nult' parameter with an incorrect value.")
  }

  # Split query
  df_queries <- NULL
  queries <- strsplit(query, split = "AND")
  for (queries_splited in queries){
    var_val <- strsplit(queries_splited, split = "=")
    for (string in var_val){
      df_queries <- rbind(df_queries, data.frame(string))
    }
  }

  # Get variables and values id
  variables <- get_variables_operation(operation)
  result <- NULL
  count <- 0
  g <- 1
  for (qvalue in df_queries$string){
    qvalue <- trimws(as.character(qvalue))
    if (count %% 2 == 0) { # variables
      variableId <- variables[match(qvalue, variables[["Nombre"]]),][["Id"]]
      variable <- paste0("g", g, "=", variableId, ":")
      result <- rbind(result, variable)
      g <- g + 1
    } else { # values
      value <- get_values_variableoperation(variableId, operation)
      value <- value[match(qvalue, value[["Nombre"]]),][["Id"]]
      if (is.na(value)){
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
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_METADATAOPERACION/", operation, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det, "&nult=", nult)

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}
