# API INE (Series)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>
#

# get_serie - SERIE
# Obtiene una serie
# [?parámetros]= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder al objeto PubFechaAct
#   tip=M para obtener los metadatos (cruce variables-valores) de la serie.
get_serie <- function(code, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE/", code, "?det=", det, "&tip=", tip)
  return(fromJSON(url))
}

# get_series_operation - SERIES_OPERACION
# Obtener series de una operación
# [?parámetros]= posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder al objeto PubFechaAct
#   tip=M para obtener los metadatos (cruce variables-valores) de la serie.
get_series_operation <- function(code, det = 0, tip = NA, page = 1, ioe = FALSE, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/IOE", code, "?page=", page, "&det=", det, "&tip=", tip)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", code, "?page=", page, "&det=", det, "&tip=", tip)
  return(fromJSON(url))
}

# get_series_values - VALORES_SERIE
# Obtiene los metadatos que definen una serie
# [?parámetros] = posibilidad de usar det=1 para acceder al objeto variable.
get_series_values <- function(code, det = 0, lang = "ES") {
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_SERIE/", code, "?det=", det)))
}

# get_series_table - SERIES_TABLA
# Obtiene las series de una tabla
# [?parámetros] =  posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder al objeto PubFechaAct
#   tip=M para obtener los metadatos (cruce variables-valores) de la serie.
get_series_table <- function(id, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_TABLA/", id, "?det=", det, "&tip=", tip)))
}

# SERIE_METADATAOPERACION
# Obtiene series mediante cruce de metadatos
# [?parámetros] =  posibilidad de usar:
#   det=2 para ver dos niveles de detalle, en contreto para poder acceder al objeto PubFechaAct
#   tip=M para obtener los metadatos (cruce variables-valores) de la serie.
get_series_metadataoperation <- function(id, g1var = 0, g1val = 0, g2var = 0, g2val = 0, g3var = 0, p = 0, det = 0, tip = NA, ioe = FALSE, lang = "ES"){
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (p < 0)
    stop("You have defined 'p' (periodicity) parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  if ((g1var <= 0) || (g1val <= 0))
    stop("You have to insert a 'g1' parameter with a correct value.")
  if ((g2var <= 0) || (g2val <= 0))
    stop("You have to insert a 'g2' parameter with a correct value.")
  if (g3var <= 0)
    stop("You have to insert a 'g3' parameter with a correct value.")
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/IOE", id, "?g1=", g1var, ":", g1val, "&g2=", g2var, ":", g2val, "&g3=", g3var, ":&p=" , p, "&tip=", tip, "&det=", det)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/", id, "?g1=", g1var, ":", g1val, "&g2=", g2var, ":", g2val, "&g3=", g3var, ":&p=" , p, "&tip=", tip, "&det=", det)
  return(fromJSON(url))
}

# Example of usage
# library(jsonlite)
# Nota: el id se obtiene del código (COD) de SERIES_OPERACION¿?
# list_serie <- get_serie("IPC206449")
# list_serie <- get_serie("IPC206449", 2, "M")
# Nota: el id se obtiene de las operaciones
# list_serie_op <- get_series_operation(25)
# list_serie_op <- get_series_operation(25, 2, "M")
# list_serie_op <- get_series_operation(30138, ioe = TRUE)
# list_serie_op <- get_series_operation(30138, 2, "M", ioe = TRUE)
# Nota: El "id" es el código identificativo de la serie (IPC206449)
# list_serie_val <- get_series_values("IPC206449")
# list_serie_val <- get_series_values("IPC206449", 1)
# Nota: El "id" es el código identificativo de la tabla (Id=22350)
# list_serie_table <- get_series_table(22350)
# list_serie_table <- get_series_table(22350, 2, "M")
# Nota:
#   Código identificativo de la operación (IOE30138 /IPC/ 25) y códigos identificativos de las variables y valores:
#   “Provincias” (FK_VARIABLE=115) = "Madrid" (FK_VALOR=29)  ⇒  g1=115:29
#   “Tipo de dato” (FK_VARIABLE=3) = “Variación mensual”   (FK_VALOR=84) ⇒ g2=3:84
#   “Grupos ECOICOP” (FK_VARIABLE=762) = "Todos los grupos ECOICOP” (FK_VALOR=null) ⇒  g3=762:
#   "Serie mensual" (FK_PERIODICIDAD=1) ⇒  p=1  (Ver PUBLICACIONES_OPERACION)
# list_serie_meta <- get_series_metadataoperation("IPC", 115, 29, 3, 84, 762, 1)
