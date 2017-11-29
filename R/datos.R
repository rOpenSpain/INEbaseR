# API INE (Datos)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# listar_datos_serie
# Return a data frame with one series data between a date range
listar_datos_serie <- function(codigo, date_start, date_end) {
  date_start <- format.Date(date_start,'%Y%m%d')
  date_end <- format.Date(date_end,'%Y%m%d')
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/DATOS_SERIE/", codigo, "?date=", date_start, ":", date_end, "&det=2")
  return(fromJSON(url)$Data)
}


# Example of usage
# library(jsonlite)
# list_datos_serie <- listar_datos_serie("FREG373", "2017-01-01", "2017-10-23")
