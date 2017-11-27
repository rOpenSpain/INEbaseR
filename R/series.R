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

# Example of usage
# library(jsonlite)
# cod <- getOperaciones()
# getSeries(cod$Codigo[1])


