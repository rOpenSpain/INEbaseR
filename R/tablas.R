# API INE (Tablas)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# listar_tablas_operacion
# Obtener todas las tablas de una operación con información geográfica
# [?parámetros] =  posibilidad de usar:
#   det=2 para ver dos niveles de profuncidad, en concreto hasta acceder al objeto PubFechaAct,
#   geo=1 para acceder sólo a tablas con contenido geográfico y
#   tip=A para que la respuesta sea de tipo amigable, en concreto el campo ultima_modificacion.
listar_tablas_operacion <- function(cod, det = 0, geo = 0) {
  if ((det < 0) || (det > 2))
    stop("Ha definido el parámetro 'det' con un valor incorrecto.")
  if ((geo < 0) || (geo > 1))
    stop("Ha definido el parámetro 'geo' con un valor incorrecto.")
  url <- paste0("http://servicios.ine.es/wstempus/js/ES/TABLAS_OPERACION/", cod, "?det=", det, "&geo=", geo)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# list_tab_op <- listar_tablas_operacion(25, 2, 1)
