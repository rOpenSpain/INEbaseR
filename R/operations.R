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

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACIONES_DISPONIBLES")

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}

#' @title Get operation
#' @description This function returns a data frame with all available statistical operations from an id or code
#' @param operation operation identificator
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @examples
#' get_operation(operation = 25)
#' get_operation(operation = "IPC")
#' get_operation(operation = 30138, ioe = TRUE)
#' @export
get_operation <- function(operation, ioe = FALSE, lang = "ES") {

  # Build URL
  if (ioe) {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACION/IOE", operation)
  } else {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACION/", operation)
  }

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)
}
