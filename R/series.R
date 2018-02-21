# API INE (Series)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get serie
#' @description This function returns a data frame with a serie from an id or code
#' @param code serie identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param lang language used to obtain information
#' @examples
#' get_serie("IPC206449")
#' get_serie("IPC206449", 2, "M")
#' @export
get_serie <- function(code, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE/", code, "?det=", det, "&tip=", tip)
  return(fromJSON(url))
  fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE/", code, "?det=", det, "&tip=", tip))
}

#' @title Get series operation
#' @description This function returns a data frame with all series of an operation from an id or code
#' @param code operation identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param page pagination
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_series_operation(25)
#' get_series_operation(25, 2, "M")
#' get_series_operation(30138, ioe = TRUE)
#' get_series_operation(30138, 2, "M", ioe = TRUE)
#' @export
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

#' @title Get series values
#' @description This function returns a data frame with the metadata that defines a series from an id or code
#' @param code serie identification
#' @param det \code{det = 1} to see the detail of the variable to which it belongs, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' get_series_values("IPC206449")
#' get_series_values("IPC206449", 1)
#' @export
get_series_values <- function(code, det = 0, lang = "ES") {
  if ((det < 0) || (det > 1))
    stop("You have defined 'det' parameter with an incorrect value.")
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_SERIE/", code, "?det=", det)))
}

#' @title Get series table
#' @description This function returns a data frame with all series of a table from an id or code
#' @param code table identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param lang language used to obtain information
#' @examples
#' get_series_table(22350)
#' get_series_table(22350, 2, "M")
#' @export
get_series_table <- function(id, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_TABLA/", id, "?det=", det, "&tip=", tip)))
}

#' @title Get series metadata operation
#' @description This function returns a data frame with all series by metadata crossing from an id or code
#' @param code operation identification
#' @param query string separated by \code{AND} with syntax \code{variable = value} using natural language
#' @param p periodicity, \code{p = 1} by default
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' Código identificativo de la operación (IOE30138 /IPC/ 25) y códigos identificativos de las variables y valores:
#' Provincias (FK_VARIABLE=115) = "Madrid" (FK_VALOR=29)  >  g1=115:29
#' Tipo de dato (FK_VARIABLE=3) = “Variación mensual”   (FK_VALOR=84) > g2=3:84
#' Grupos ECOICOP (FK_VARIABLE=762) = "Todos los grupos ECOICOP” (FK_VALOR=null) >  g3=762:
#' Serie mensual (FK_PERIODICIDAD=1) >  p=1  (Ver PUBLICACIONES_OPERACION)
#' @examples
#' get_series_metadataoperation("IPC", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL")
#' @export
get_series_metadataoperation <- function(code, query = NULL, p = 1, det = 0, tip = NA, ioe = FALSE, lang = "ES"){

  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if (p <= 0)
    stop("You have defined 'p' (periodicity) parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")

  # Split query
  df_queries <- NULL
  queries <- strsplit(query, split = "AND")
  for (queries_splited in queries){
    var_val <- strsplit(queries_splited, split = "=")
    for (string in var_val){
      df_queries <- rbind(df_queries, data.frame(string))
    }
  }

  # Get variables and values id
  variables <- get_variables_operation(code)
  result <- NULL
  count <- 0
  g <- 1
  for (qvalue in df_queries$string){
    qvalue <- trimws(as.character(qvalue))
    if (count %% 2 == 0) { # variables
      variableId <- variables[match(qvalue, variables[["Nombre"]]),][["Id"]]
      variable <- paste0("g", g, "=", variableId, ":")
      result <- rbind(result, variable)
      g <- g + 1
    } else { # values
      value <- get_values_variableoperation(variableId, code)
      value <- value[match(qvalue, value[["Nombre"]]),][["Id"]]
      if (is.na(value)){
        value <- ""
      }
      value <- paste0(value, "&")
      result <- rbind(result, value)
    }
    count <- count + 1
  }

  # Join query
  urlStr <- paste0(result, collapse = "")

  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/IOE", code, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/", code, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det)
  return(fromJSON(url))
}

#' @title Plot series
#' @description This function draws a plot with data of a series from an id and/or from a date or date range
#' @param code identification code of a serie (e.g. "IPC206449")
#' @param date_start start date in format (string) \code{YYYY-MM-DD}
#' @param date_end end date in format (string) \code{YYYY-MM-DD}
#' @param nult last \code{n} values
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param type what type of plot should be drawn, \code{type = "p"} (for points) by default
#' @param lang language used to obtain information
#' @examples
#' plot_series("IPC206449", nult = 1) # Get the latest data of a series
#' plot_series("IPC206449", nult = 5, type = "l") # Get the \code{n} last data of a series
#' plot_series("IPC206449", "2013-01-01", "2016-01-01") # Get data of a series between two dates
#' plot_series("IPC206449", "2010-01-01") # Get data from a series from a date
#' @export
plot_series <- function(code, date_start = NA, date_end = NA, nult = 0, det = 0, type = NA, lang = "ES") {
  data <- get_data_serie(code, date_start, date_end, nult, det, lang)$Data
  if (is.na(type))
    type = "p"
  plot(x = as.POSIXct((data$Fecha+0.1)/1000, origin = "1960-01-01", tz = "GMT"), y = data$Valor, type = type)
  title(main = paste("Datos de la serie", code), xlab = "Fechas", ylab = "Datos")
}
