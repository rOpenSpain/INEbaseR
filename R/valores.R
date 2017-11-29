# API INE (Valores)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# listar_valores_all
# Información de los valores que puede tomar la variable: identificador Tempus3 del valor, identificador Tempus 3 de la variable a la que pertenece, nombre del valor y código oficial.
# [?parámetros] = posibilidad de usar det=1 para ver el detalle de la variable a la que pertenece.
listar_valores_all <- function(id, det = 0){
  if ((det > 1) || (det < 0))
    stop("Ha definido el parámetro 'det' con un valor incorrecto.")
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLE/", id, "?det=", det)
  return(fromJSON(url))
}

# listar_valores_variableoperacion
# Información de los valores que puede tomar la variable para describir la operación: identificador Tempus3 del valor, objeto variable Tempus3 a la que pertenece, nombre del valor y código oficial.
# [?parámetros] = posibilidad de usar det=1 para ver el detalle de la variable a la que pertenece.
listar_valores_variableoperacion <- function(id, op, det = 0) {
  if ((det > 1) || (det < 0))
    stop("Ha definido el parámetro 'det' con un valor incorrecto.")
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/", id, "/", op, "?det=", det)
  return(fromJSON(url))
}

# Example of usage
# list_val <- listar_valores_all(115)
# list_val <- listar_valores_all(115, 1)
# list_val_var_op <- listar_valores_variableoperacion(762, 25)
# list_val_var_op <- listar_valores_variableoperacion(762, 25, 1)
