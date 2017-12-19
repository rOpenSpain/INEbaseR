# API INE (Series)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get serie
#' @description This function returns a data frame with a serie from an id or code
#' @param code serie identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param lang language used to obtain information
#' @examples
#' get_serie("IPC206449")
#' get_serie("IPC206449", 2, "M")
#' @export
get_serie <- function(code, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE/", code, "?det=", det, "&tip=", tip)
  return(fromJSON(url))
}

#' @title Get series operation
#' @description This function returns a data frame with all series of an operation from an id or code
#' @param code operation identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param page pagination
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_series_operation(25)
#' get_series_operation(25, 2, "M")
#' get_series_operation(30138, ioe = TRUE)
#' get_series_operation(30138, 2, "M", ioe = TRUE)
#' @export
get_series_operation <- function(code, det = 0, tip = NA, page = 1, ioe = FALSE, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/IOE", code, "?page=", page, "&det=", det, "&tip=", tip)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", code, "?page=", page, "&det=", det, "&tip=", tip)
  return(fromJSON(url))
}

#' @title Get series values
#' @description This function returns a data frame with the metadata that defines a series from an id or code
#' @param code serie identification
#' @param det \code{det = 1} to see the detail of the variable to which it belongs, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' get_series_values("IPC206449")
#' get_series_values("IPC206449", 1)
#' @export
get_series_values <- function(code, det = 0, lang = "ES") {
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_SERIE/", code, "?det=", det)))
}

#' @title Get series table
#' @description This function returns a data frame with all series of a table from an id or code
#' @param code table identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param lang language used to obtain information
#' @examples
#' get_series_table(22350)
#' get_series_table(22350, 2, "M")
#' @export
get_series_table <- function(id, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_TABLA/", id, "?det=", det, "&tip=", tip)))
}

#' @title Get series metadata operation
#' @description This function returns a data frame with all series by metadata crossing from an id or code
#' @param code operation identification
#' @param g1var variable (g1)
#' @param g1val valor (g1)
#' @param g2var variable (g2)
#' @param g2val valor (g2)
#' @param g3var variable (g3)
#' @param p periodicidad
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' Código identificativo de la operación (IOE30138 /IPC/ 25) y códigos identificativos de las variables y valores:
#' Provincias (FK_VARIABLE=115) = "Madrid" (FK_VALOR=29)  ⇒  g1=115:29
#' Tipo de dato (FK_VARIABLE=3) = “Variación mensual”   (FK_VALOR=84) ⇒ g2=3:84
#' Grupos ECOICOP (FK_VARIABLE=762) = "Todos los grupos ECOICOP” (FK_VALOR=null) ⇒  g3=762:
#' Serie mensual (FK_PERIODICIDAD=1) ⇒  p=1  (Ver PUBLICACIONES_OPERACION)
#' @examples
#' get_series_metadataoperation("IPC", 115, 29, 3, 84, 762, 1)
#' @export
get_series_metadataoperation <- function(id, g1var = 0, g1val = 0, g2var = 0, g2val = 0, g3var = 0, p = 0, det = 0, tip = NA, ioe = FALSE, lang = "ES"){
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (p < 0)
    stop("You have defined 'p' (periodicity) parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if ((g1var <= 0) || (g1val <= 0))
    stop("You have to insert a 'g1' parameter with a correct value.")
  if ((g2var <= 0) || (g2val <= 0))
    stop("You have to insert a 'g2' parameter with a correct value.")
  if (g3var <= 0)
    stop("You have to insert a 'g3' parameter with a correct value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/IOE", id, "?g1=", g1var, ":", g1val, "&g2=", g2var, ":", g2val, "&g3=", g3var, ":&p=" , p, "&tip=", tip, "&det=", det)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/", id, "?g1=", g1var, ":", g1val, "&g2=", g2var, ":", g2val, "&g3=", g3var, ":&p=" , p, "&tip=", tip, "&det=", det)
  return(fromJSON(url))
}
