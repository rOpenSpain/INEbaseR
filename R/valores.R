# API INE (Valores)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# get_values_all
# Obtiene todos los valores de una variable.
# Consulta: Valores de la variable “Provincias”.
# [?parámetros] = posibilidad de usar det=1 para ver el detalle de la variable a la que pertenece.
get_values_all <- function(id, det = 0, lang = "ES") {
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLE/", id, "?det=", det)
  return(fromJSON(url))
}

# get_values_variableoperation
# Obtiene todos los valores de una variable para una operación.
# [?parámetros] = posibilidad de usar det=1 para ver el detalle de la variable a la que pertenece.
get_values_variableoperation <- function(id, op, det = 0, ioe = FALSE, lang = "ES") {
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLEOPERACION/", id, "/IOE", op, "?det=", det)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLEOPERACION/", id, "/", op, "?det=", det)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# Nota: estos id (115) se obtienen de las variables ("Provincias")
# list_val <- get_values_all(115) # "det" no definido.
# list_val <- get_values_all(115, 1) # det = 1
# Nota: el "id" (762) se obtiene del código id de variables, y "op" (25) es el código de las operaciones
# list_val_var_op <- get_values_variableoperation(762, 25)
# list_val_var_op <- get_values_variableoperation(762, 25, 1)
# list_val_var_op <- get_values_variableoperation(762, 30138, ioe = TRUE)
# list_val_var_op <- get_values_variableoperation(762, 30138, ioe = TRUE, 1)
