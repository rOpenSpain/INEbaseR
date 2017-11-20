# API INE (Series)
#
# Author: Andrés Nacimiento García <andresnacimiento@gmail.com>
# Director de proyecto: Carlos Pérez González <cpgonzal@ull.es>
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
# cod <- getOperaciones()
# getSeries(cod$Codigo[1])


