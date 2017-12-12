# API INE (Publicaciones)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# get_publications - PUBLICACIONES
# Obtiene todas las publicaciones
# [?parámetros]= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder a los atributos del objeto PubFechaAct.
get_publications <- function(det = 0, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES?det=", det)
  return(fromJSON(url))
}

# get_publications_operation - PUBLICACIONES_OPERACION
# Obtiene las publicaciones de una operación
# [?parámetros]= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder a los atributos del objeto PubFechaAct.
get_publications_operation <- function(code, ioe = FALSE, det = 0, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES_OPERACION/IOE", code, "?det=", det)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES_OPERACION/", code, "?det=", det)
  return(fromJSON(url))
}

# get_publications_date - PUBLICACIONFECHA_PUBLICACION
# Obtiene las fechas de publicación de una publicación
# [?parámetros]= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder a los atributos del objeto PubFechaAct.
get_publications_date <- function(id, det = 0, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONFECHA_PUBLICACION/", id, "?det=", det)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# list_pub <- get_publicaciones()
# list_pub_op <- get_publications_operation(25)
# list_pub_op <- get_publications_operation(30138, ioe = TRUE)
# Nota: El "id" es el código identificativo de la publicación (Id=8).
# list_pub_date <- get_publications_date(8)
