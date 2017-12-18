# API INE (Variables)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get variables (all)
#' @description This function returns a data frame with all system variables
#' @param lang language used to obtain information
#' @examples
#' get_variables_all()
#' @export
get_variables_all <- function(lang = "ES") {
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES")))
}

#' @title Get variable
#' @description This function returns a data frame with system variables of an operation from an id or code
#' @param code operation identification
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_variable(25)
#' get_variable("IPC")
#' get_variable(30138, ioe = TRUE)
#' @export
get_variable <- function(code, ioe = FALSE, lang = "ES") {
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES_OPERACION/IOE", code)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES_OPERACION/", code)
  return(fromJSON(url))
}
