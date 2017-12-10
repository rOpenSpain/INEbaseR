# API INE (Tablas)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# get_tables_operation - TABLAS_OPERACION
# Obtiene todas las tablas de una operación con información geográfica
# [?parámetros] =  posibilidad de usar:
#   det=2 para ver dos niveles de profuncidad, en concreto hasta acceder al objeto PubFechaAct,
#   geo=1 para acceder sólo a tablas con contenido geográfico y
#   tip=A para que la respuesta sea de tipo amigable, en concreto el campo ultima_modificacion.
get_tables_operation <- function(cod, det = 0, geo = 0, tip = NA, ioe = FALSE, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((geo < 0) || (geo > 1))
    stop("You have defined 'geo' parameter with an incorrect value.")
  if ((tip != "A") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/TABLAS_OPERACION/IOE", cod, "?det=", det, "&geo=", geo, "&tip=", tip)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/TABLAS_OPERACION/", cod, "?det=", det, "&geo=", geo, "&tip=", tip)
  return(fromJSON(url))
}

# get_tables_group - GRUPOS_TABLA
# Obtiene la combinación de variables y valores que definen una tabla. (primera parte)
get_tables_group <- function(id, lang = "ES"){
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/GRUPOS_TABLA/", id)))
}

# get_tables_group_values - VALORES_GRUPOSTABLA
# Obtiene la combinación de variables y valores que definen una tabla. (segunda parte)
# [?parámetros] = posibilidad de usar det=1 para ver el detalle de la variable a la que pertenece.
get_tables_group_values <- function(id, grp, det = 0, lang = "ES"){
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_GRUPOSTABLA/", id, "/", grp, "?det=", det)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# Nota: el "id" se obtiene de las operaciones
# list_tables_op <- get_tables_operation(25)
# list_tables_op <- get_tables_operation(25, 2, 1, tip = "A")
# list_tables_op <- get_tables_operation(30138, ioe = TRUE, 2, 1, tip = "A")
# Nota: código identificativo de la tabla (Id=22350).
# list_tables_grp <- get_tables_group(22350)
# Nota: códigos identificativos de la tabla (Id=22350) y del grupo "Comunidades y Ciudades Autónomas" (Id=81497).
# list_tables_grp_val <-  get_tables_group_values(22350, 81497)
# list_tables_grp_val <- get_tables_group_values(22350, 81497, 1)
