# API INE (Series)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# getOperaciones
# Return all operations available
getOperaciones <- function() {
  return(fromJSON("http://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES"))
}

# getSeries
# Return all series from an operation
getSeries <- function(codigo) {
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/SERIES_OPERACION/", codigo, "?page=1&det=2")
  return(fromJSON(url))
}

# getSerie
# Return a data frame with one series data between a date range
getSerie <- function(codigo, date_start, date_end) {
  date_start <- format.Date(date_start,'%Y%m%d')
  date_end <- format.Date(date_end,'%Y%m%d')
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/DATOS_SERIE/", codigo, "?date=", date_start, ":", date_end, "&det=2")
  return(fromJSON(url)$Data)
}


# Example of usage
# library(jsonlite)
# codOperaciones <- getOperaciones()
# codeSeries <- getSeries(codOperaciones$Codigo[1])
# codSerieDate <- getSerie(codeSeries$COD[1], "2017-01-01", "2017-10-23")
# codSerieDate <- getSerie("FREG373", "2017-01-01", "2017-10-23")

