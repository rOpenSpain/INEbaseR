# API INE (Datos)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get data serie
#' @description This function returns a data frame with data of a serie from an id or/and from a date or date range
#' @param id operation identification
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
get_data_serie <- function(id, date_start = NA, date_end = NA, nult = 0, det = 0, lang = "ES") {
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
#' @param id table identification
#' @param g1var variable (g1)
#' @param g1val valor (g1)
#' @param g2var variable (g2)
#' @param g2val valor (g2)
#' @param g3var variable (g3)
#' @param p periodicidad
#' @param nult last \code{n} values
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = AM} to obtain the metadata (crossing variables-values) of the series and a friendly output.
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @examples
#' get_data_metadataoperation("IPC", 115, 29, 3, 84, 762, p = 1, nult = 1)
#' @export
get_data_metadataoperation <- function(id, g1var = 0, g1val = 0, g2var = 0, g2val = 0, g3var = 0, p = 0, nult = 0, det = 0, tip = NA, ioe = FALSE, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (p < 0)
    stop("You have defined 'p' (periodicity) parameter with an incorrect value.")
  if ((tip != "AM") || (is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if ((g1var <= 0) || (g1val <= 0))
    stop("You have to insert a 'g1' parameter with a correct value.")
  if ((g2var <= 0) || (g2val <= 0))
    stop("You have to insert a 'g2' parameter with a correct value.")
  if (g3var <= 0)
    stop("You have to insert a 'g3' parameter with a correct value.")
  if (ioe)
    if (nult == 0)
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_METADATAOPERACION/IOE", id, "?g1=", g1var, ":", g1val, "&g2=", g2var, ":", g2val, "&g3=", g3var, ":&p=" , p, "&tip=", tip, "&det=", det)
    else
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_METADATAOPERACION/IOE", id, "?nult=", nult, "&g1=", g1var, ":", g1val, "&g2=", g2var, ":", g2val, "&g3=", g3var, ":&p=" , p, "&tip=", tip, "&det=", det)
  else
    if (nult == 0)
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_METADATAOPERACION/", id, "?g1=", g1var, ":", g1val, "&g2=", g2var, ":", g2val, "&g3=", g3var, ":&p=" , p, "&tip=", tip, "&det=", det)
    else
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/DATOS_METADATAOPERACION/", id, "?nult=", nult, "&g1=", g1var, ":", g1val, "&g2=", g2var, ":", g2val, "&g3=", g3var, ":&p=" , p, "&tip=", tip, "&det=", det)
  return(fromJSON(url))
}
