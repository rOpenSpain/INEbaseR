# API INE (Publicaciones)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get publications
#' @description This function returns a data frame with all available publications from an id or code
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' get_publications()
#' @export
get_publications <- function(det = 0, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES?det=", det)
  return(fromJSON(url))
}

#' @title Get publications operation
#' @description This function returns a data frame with publications of an operation from an id or code
#' @param code operation identification
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_publications_operation(25)
#' get_publications_operation(30138, ioe = TRUE)
#' @export
get_publications_operation <- function(code, ioe = FALSE, det = 0, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES_OPERACION/IOE", code, "?det=", det)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES_OPERACION/", code, "?det=", det)
  return(fromJSON(url))
}

#' @title Get publications date
#' @description This function returns a data frame with all publication date of a publication from an id or code
#' @param code publication identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' get_publications_date(8)
#' @export
get_publications_date <- function(code, det = 0, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONFECHA_PUBLICACION/", code, "?det=", det)
  return(fromJSON(url))
}
