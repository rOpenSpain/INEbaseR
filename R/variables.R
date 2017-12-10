# API INE (Variables)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# get_variables_all
# Obtiene todas las variables del Sistema.
get_variables_all <- function(lang = "ES") {
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES")))
}

# get_variable
# Obtiene un data frame con las variables para una operación a partir de:
#   Código numérico (id)
#   código alfabético (IPC)
#   código IOE (Inventario de Operaciones Estadísticas)
get_variable <- function(code, ioe = FALSE, lang = "ES") {
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES_OPERACION/IOE", code)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES_OPERACION/", code)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# list_var_all <- get_variables_all()
# Nota: estos códigos se obtienen de las operaciones
# list_var_id <- get_variable(25)
# list_var_cod <- get_variable("IPC")
# list_var_ioe <- get_variable(30138, ioe = TRUE)
