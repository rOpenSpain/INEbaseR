# API INE (Series)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# get_serie - SERIE
# Obtiene una serie
# [?parámetros]= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder al objeto PubFechaAct
#   tip=M para obtener los metadatos (cruce variables-valores) de la serie.
get_serie <- function(code, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE/", code, "?det=", det, "&tip=", tip)
  return(fromJSON(url))
}

# get_series_operation - SERIES_OPERACION
# Obtener series de una operación
# [?parámetros]= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder al objeto PubFechaAct
#   tip=M para obtener los metadatos (cruce variables-valores) de la serie.
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

# Example of usage
# library(jsonlite)
# Nota: el id se obtiene del código (COD) de SERIES_OPERACION¿?
# list_serie <- get_serie("IPC206449")
# list_serie <- get_serie("IPC206449", 2, "M")
# Nota: el id se obtiene de las operaciones
# list_serie_op <- get_series_operation(25)
# list_serie_op <- get_series_operation(25, 2, "M")
# list_serie_op <- get_series_operation(30138, ioe = TRUE)
# list_serie_op <- get_series_operation(30138, 2, "M", ioe = TRUE)
