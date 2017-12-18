# API INE (Valores)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get values (all)
#' @description This function returns a data frame with all values from a variable
#' @param id operation identification
#' @param det \code{det = 1} to see the detail of the variable to which it belongs, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' get_values_all(115)
#' get_values_all(115, 1)
#' @export
get_values_all <- function(id, det = 0, lang = "ES") {
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLE/", id, "?det=", det)
  return(fromJSON(url))
}

#' @title Get values from variable operation
#' @description This function returns a data frame with all values from a variable to an operation
#' @param id variable identification
#' @param op operation identification
#' @param det \code{det = 1} to see the detail of the variable to which it belongs, \code{det = 0} by default
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_values_variableoperation(762, 25)
#' get_values_variableoperation(762, 25, 1)
#' get_values_variableoperation(762, 30138, ioe = TRUE)
#' get_values_variableoperation(762, 30138, ioe = TRUE, 1)
#' @export
get_values_variableoperation <- function(id, op, det = 0, ioe = FALSE, lang = "ES") {
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLEOPERACION/", id, "/IOE", op, "?det=", det)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLEOPERACION/", id, "/", op, "?det=", det)
  return(fromJSON(url))
}
