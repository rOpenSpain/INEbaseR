# API INE (Datos)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# get_data_serie - DATOS_SERIE
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

# get_data_table - DATOS_TABLA
# Obtiene los n últimos datos de una tabla
# {?parámetros}= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en concreto para poder acceder a los atributos del objeto dato
#   tip=AM para obtener los metadatos (cruce variables-valores) de la serie y una salida de tipo amigable.
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

# Example of usage
# library(jsonlite)
# DATOS_SERIE:
# Obtener el último dato de una serie
# list_datos_serie <- get_data_serie("IPC206449", nult = 1)
# Obtener los n últimos datos de una serie
# list_datos_serie <- get_data_serie("IPC206449", nult = 5)
# Obtener datos de una serie entre dos fechas
# list_datos_serie <- get_data_serie("IPC206449", "2013-01-01", "2016-01-01")
# Obtener datos de una serie a partir de una fecha
# list_datos_serie <- get_data_serie("IPC206449", "2010-01-01")
# Obtener los n últimos datos de una tabla
# list_datos_tabla <- get_data_table(22350, 4)
