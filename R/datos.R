# API INE (Datos)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get data serie
#' @description This function returns a data frame with data of a serie from an id or/and from a date or date range
#' @param code operation identification
#' @param date_start start date in format (string) \code{YYYY-MM-DD}
#' @param date_end end date in format (string) \code{YYYY-MM-DD}
#' @param nult last \code{n} values
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' get_data_serie("IPC206449", nult = 1) # Get the latest data of a series
#' get_data_serie("IPC206449", nult = 5) # Get the \code{n} last data of a series
#' get_data_serie("IPC206449", "2013-01-01", "2016-01-01") # Get data of a series between two dates
#' get_data_serie("IPC206449", "2010-01-01") # Get data from a series from a date
#' @export
get_data_serie <- function(code, date_start = NA, date_end = NA, nult = 0, det = 0, lang = "ES") {
  if (!is.na(date_end))
    if (date_start > date_end)
      stop("Start date cannot be after the end date.")
  date_start <- format.Date(date_start,'%Y%m%d')
  date_end <- format.Date(date_end,'%Y%m%d')
  if (nult == 0) {
    if (is.na(date_end))
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_SERIE/", code, "?date=", date_start, ":&det=", det)
    else
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_SERIE/", code, "?date=", date_start, ":", date_end, "&det=", det)
  } else {
    if (is.na(date_end))
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_SERIE/", code, "?nult=", nult, "&date=", date_start, ":&det=", det)
    else
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_SERIE/", code, "?nult=", nult, "&date=", date_start, ":", date_end, "&det=", det)
  }
  return(fromJSON(url))
}

#' @title Get data table
#' @description This function returns a data frame with latest \code{n} data of a table from an id or code
#' @param id table identification
#' @param nult last \code{n} values
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = AM} to obtain the metadata (crossing variables-values) of the series and a friendly output.
#' @param lang language used to obtain information
#' @examples
#' get_data_table(22350, 4)
#' @export
get_data_table <- function(id, nult = 0, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "AM") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if (nult == 0)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_TABLA/", id)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_TABLA/", id, "?nult=", nult)
  return(fromJSON(url))
}

#' @title Get data metadata-operation
#' @description This function returns a data frame with latest \code{n} series data by crossing metadata from an id or code
#' @param code table identification
#' @param query string separated by \code{AND} with syntax \code{variable = value} using natural language
#' @param p periodicity
#' @param nult last \code{n} values
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = AM} to obtain the metadata (crossing variables-values) of the series and a friendly output.
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @examples
#' get_data_metadataoperation("IPC", 115, 29, 3, 84, 762, p = 1, nult = 1)
#' get_data_metadataoperation("IPC", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL", nult = 1)
#' @export
get_data_metadataoperation <- function(code, query = NULL, p = 1, nult = 1, det = 0, tip = NA, ioe = FALSE, lang = "ES") {

  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (p < 0)
    stop("You have defined 'p' (periodicity) parameter with an incorrect value.")
  if ((tip != "AM") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if (nult < 1)
    stop("You have defined 'nult' parameter with an incorrect value.")

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
  variables <- get_variables_operation(code)
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
      value <- get_values_variableoperation(variableId, code)
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

  # URL definition
  if (ioe)
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_METADATAOPERACION/IOE", code, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det, "&nult=", nult)
  else
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_METADATAOPERACION/", code, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det, "&nult=", nult)

  print(url)
  return(fromJSON(url))
}
