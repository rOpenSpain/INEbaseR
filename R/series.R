# API INE (Series)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# listar_operaciones_series
# Return all operations available
listar_operaciones_series <- function() {
  return(fromJSON("http://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES"))
}

# obtener_serie
# Obtiene una serie a partir del código identificativo de la serie
obtener_serie <- function(codigo) {
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/SERIE/", codigo)
  return(fromJSON(url))
}


# Example of usage
# ob_serie <- obtener_serie("IPC206449")




