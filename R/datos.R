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

# DATOS_METADATAOPERACION
# Obtiene los "n" últimos datos de series mediante el cruce de metadatos
# {?parámetros}= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en concreto para poder acceder a los atributos del objeto dato.
#   tip=AM para obtener los metadatos (cruce variables-valores) de la serie y una salida de tipo amigable.
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
# DATOS_TABLA:
# Obtener los n últimos datos de una tabla
# list_datos_tabla <- get_data_table(22350, 4)
# DATOS_METADATAOPERACION:
# Obtener n los últimos datos de series mediante el cruce de metadatos
# list_datos_metaop <- get_data_metadataoperation("IPC", 115, 29, 3, 84, 762, p = 1, nult = 1)
