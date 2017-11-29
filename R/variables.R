# API INE (Variables)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#


# listar_variables_all
# Información de todas las variables del Sistema: identificador Tempus3, nombre de la variable y código oficial.
listar_variables_all <- function() {
  return(fromJSON("http://servicios.ine.es/wstempus/js/ES/VARIABLES"))
}

# listar_variables_operacion
# Información de las variables que describen la operación: identificador Tempus3, nombre de la variable y código oficial.
listar_variables_operacion <- function(cod) {
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/", cod)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# list_vars <- listar_variables_all()
# list_vars_op <- listar_variables_operacion("IOE30138")
