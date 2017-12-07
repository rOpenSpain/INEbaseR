# API INE (Operaciones)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# get_operations_all
# Obtiene un data frame con todas las operaciones estadísticas disponibles
get_operations_all <- function(lang = "ES") {
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACIONES_DISPONIBLES")))
}

# get_operation
# Obtiene un data frame a partir de:
#   Código numérico (id)
#   código alfabético (IPC)
#   código IOE (Inventario de Operaciones Estadísticas)
get_operation <- function(code, ioe = FALSE, lang = "ES") {
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACION/IOE", code)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACION/", code)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# list_op_all <- get_operations_all()
# list_op_id <- get_operation(25)
# list_op_cod <- get_operation("IPC")
# list_op_ioe <- get_operation(30138, ioe = TRUE)
