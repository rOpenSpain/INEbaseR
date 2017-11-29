# API INE (Series)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# obtener_serie
# Obtiene una serie a partir del código identificativo de la serie
obtener_serie <- function(cod) {
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/SERIE/", cod)
  return(fromJSON(url))
}

# listar_series_operacion
# Obtiene series de una operación
listar_series_operacion <- function(cod) {
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/SERIES_OPERACION/", cod)
  return(fromJSON(url))
}

# Example of usage
# list_serie_op <- listar_series_operacion("IPC")
# ob_serie <- obtener_serie("IPC206449")




