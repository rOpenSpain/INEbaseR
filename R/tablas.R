# API INE (Tablas)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get tables operation
#' @description This function returns a data frame with all tables of an operation with geographic information from an id or code
#' @param code operation identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param geo \code{geo = 1} to access only tables with geographic content, \code{geo = 0} by default
#' @param tip \code{tip = A} to view as friendly, specifically the view the field \code{ultima_modificacion}
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_tables_operation(25)
#' get_tables_operation(25, 2, 1, tip = "A")
#' get_tables_operation(30138, ioe = TRUE, 2, 1, tip = "A")
#' @export
get_tables_operation <- function(code, det = 0, geo = 0, tip = NA, ioe = FALSE, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((geo < 0) || (geo > 1))
    stop("You have defined 'geo' parameter with an incorrect value.")
  if ((tip != "A") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/TABLAS_OPERACION/IOE", code, "?det=", det, "&geo=", geo, "&tip=", tip)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/TABLAS_OPERACION/", code, "?det=", det, "&geo=", geo, "&tip=", tip)
  return(fromJSON(url))
}

#' @title Get tables group
#' @description This function returns a data frame with a combination of variables and values that define a table from an id or code
#' @param id table identification
#' @param lang language used to obtain information
#' @examples
#' get_tables_group(22350)
#' @export
get_tables_group <- function(id, lang = "ES"){
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/GRUPOS_TABLA/", id)))
}

# get_tables_group_values - VALORES_GRUPOSTABLA
# Obtiene la combinación de variables y valores que definen una tabla. (segunda parte)
#' @title Get tables group values
#' @description This function returns a data frame with a combination of variables and values that define a table from an id or code
#' @param id table identification
#' @param grp group identification
#' @param det \code{det = 1} to see the detail of the variable to which it belongs, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' get_tables_group_values(22350, 81497)
#' get_tables_group_values(22350, 81497, 1)
#' @export
get_tables_group_values <- function(id, grp, det = 0, lang = "ES"){
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_GRUPOSTABLA/", id, "/", grp, "?det=", det)
  return(fromJSON(url))
}
