# API INE (Operaciones)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get operations (all)
#' @description This function returns a data frame with all available statistical operations
#' @param lang language used to obtain information
#' @examples
#' get_operations_all()
#' @export
get_operations_all <- function(lang = "ES") {
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACIONES_DISPONIBLES")))
}

#' @title Get operation
#' @description This function returns a data frame with all available statistical operations from an id or code
#' @param code operation identification
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_operation(25)
#' get_operation("IPC")
#' get_operation(30138, ioe = TRUE)
#' @export
get_operation <- function(code, ioe = FALSE, lang = "ES") {
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACION/IOE", code)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACION/", code)
  return(fromJSON(url))
}
