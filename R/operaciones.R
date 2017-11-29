# API INE (Operaciones)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# listar_operaciones_all
# Obtiene un data frame con todas las operaciones estadísticas disponibles
listar_operaciones_all <- function() {
  return(fromJSON("http://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES"))
}

# listar_operaciones_id
# Obtiene un data frame a partir del código numérico Tempus3 interno (id)
listar_operaciones_id <- function(id) {
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/TABLAS_OPERACION/", id)
  return(fromJSON(url))
}

# listar_operaciones_ioe
# Obtiene un data frame a partir del IOE (Inventario de Operaciones Estadísticas)
listar_operaciones_ioe <- function(cod_ioe) {
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/TABLAS_OPERACION/IOE", cod_ioe)
  return(fromJSON(url))
}

# listar_operaciones_codigo
# Obtiene un data frame a partir del código alfabético Tempus3 interno
listar_operaciones_codigo <-function(cod) {
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/TABLAS_OPERACION/", cod)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# list_op_all <- listar_operaciones_all()
# list_op_id <- listar_operaciones_id(25)
# list_op_ioe <- listar_operaciones_ioe(30138)
# list_op_cod <- listar_operaciones_codigo("IPC")
