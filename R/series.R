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
#' get_serie("IPC206449", det = 2, tip = "M")
#' @export
get_serie <- function(code, det = 0, tip = NA, lang = "ES") {

  # Checking options
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")

  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")

  # URL definition
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE/", code, "?det=", det, "&tip=", tip)

  # Get data
  data <- fromJSON(url)

  return(data)
}

#' @title Get series operation
#' @description This function returns a data frame with all series of an operation from an id or code
#' @param code operation identification
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param pagination \code{TRUE} to obtain data page by page and \code{FALSE} by default.
#' @param page \code{page = 1} to obtain data of an specific page (to use this, \code{pagination = FALSE}).
#' @param page_start \code{page_start = 1} start page range to obtain data (to use this, \code{pagination = TRUE}).
#' @param page_end \code{page_end = 2} end page range to obtain data (to use this, \code{pagination = TRUE}).
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @param cache used to load data from local cache instead API, \code{cache = FALSE} by default.
#' @param benchmark used to measure the performance of the system, \code{benchmark = FALSE} by default.
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_series_operation(25)
#' get_series_operation(25, 2, "M")
#' get_series_operation(30138, ioe = TRUE)
#' get_series_operation(30138, 2, "M", ioe = TRUE)
#' get_series_operation("IPC", det = 2, tip = "M")
#' get_series_operation(25, pagination = FALSE, page = 1, cache = FALSE)
#' get_series_operation(25, pagination = TRUE, page_start = 1, page_end = 2, cache = FALSE)
#' @export
get_series_operation <- function(code, det = 0, tip = NA, pagination = FALSE, page = NA, page_start = NA, page_end = NA, ioe = FALSE, lang = "ES", cache = TRUE, benchmark = FALSE) {

  # Checking options
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")

  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")

  # Start the clock!
  if (benchmark) {
    rnorm(100000)
    rep(NA, 100000)
    ptm <- proc.time()
  }

  # Convert code to ID
  operations <- get_operations_all()
  if (class(code) == "character") {
    code <- operations[operations$Codigo == code,]$Id
  } else {
    if (ioe) {
      code <- operations[operations$Cod_IOE == code,]$Id
    }
  }

  # Get data from cache
  if (cache) {
    data <- get_cache("SERIEOPERATION", code)
  }

  # Get data from API
  else {

    data <- NULL

    if (pagination) {

      empty_content <- FALSE
      page <- 1

      if (!is.na(page_start)) {
        if (page_start <= 0) {
          stop("You have defined 'page_start' parameter with an incorrect value.")
        } else {
          page <- page_start
        }
      }

      while (!empty_content) {

        # URL definition
        if (ioe) {
          url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/IOE", code, "?page=", page, "&det=", det, "&tip=", tip)
        }
        else {
          url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", code, "?page=", page, "&det=", det, "&tip=", tip)
        }

        print(url)

        content <- fromJSON(url)
        if (length(content) == 0) {
          empty_content <- TRUE
          next
          # print(paste0("No content found in page ", page))
        } else {

          data_content <- NULL
          for (i in 1:nrow(content)) {
            data_content$COD <- rbind(data_content$COD, content$COD[i])
            data_content$T3_Operacion <- rbind(data_content$T3_Operacion, content$T3_Operacion[i])
            data_content$Nombre <- rbind(data_content$Nombre, content$Nombre[i])
            data_content$Decimales <- rbind(data_content$Decimales, content$Decimales[i])
            data_content$T3_Periodicidad <- rbind(data_content$T3_Periodicidad, content$T3_Periodicidad[i])
            data_content$T3_Publicacion <- rbind(data_content$T3_Publicacion, content$T3_Publicacion[i])
            data_content$T3_Clasificacion <- rbind(data_content$T3_Clasificacion, content$T3_Clasificacion[i])
            data_content$T3_Escala <- rbind(data_content$T3_Escala, content$T3_Escala[i])
            data_content$T3_Unidad <- rbind(data_content$T3_Unidad, content$T3_Unidad[i])
            # Metadata
            if ((tip == "M") && (!is.null(content$Periodicidad$Nombre))) {
              data_content$Periodicidad <- rbind(data_content$Periodicidad, content$Periodicidad$Nombre[i])
            }

          }
        }

        # Convert to data frame
        data_content <- data.frame(data_content, stringsAsFactors = FALSE)

        # Build data content
        data <- rbind(data, data_content)

        if (!is.na(page_end)) {
          if (page == page_end)
            break
        }

        page <- page + 1

      }

      # Convert to data frame
      data <- data.frame(data, stringsAsFactors = FALSE)

    # Get all data
    } else {

      if (is.na(page)) {
        # URL definition
        if (ioe) {
          url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/IOE", code, "?page=", NULL, "&det=", det, "&tip=", tip)
        }
        else {
          url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", code, "?page=", NULL, "&det=", det, "&tip=", tip)
        }
      } else {
        # URL definition
        if (ioe) {
          url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/IOE", code, "?page=", page, "&det=", det, "&tip=", tip)
        }
        else {
          url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", code, "?page=", page, "&det=", det, "&tip=", tip)
        }
      }

      data <- fromJSON(url)
    }

    build_cache(data, "SERIEOPERATION", code)

  }

  # Stop the clock
  if (benchmark) {
    print(proc.time() - ptm)
  }

  return(data)
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
get_series_table <- function(code, det = 0, tip = NA, lang = "ES") {
  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_TABLA/", code, "?det=", det, "&tip=", tip)))
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
get_series_metadataoperation <- function(code, query = NULL, p = NULL, det = 0, tip = NA, ioe = FALSE, lang = "ES"){

  if ((det < 0) || (det > 2))
    stop("You have defined 'det' parameter with an incorrect value.")
  if ((p <= 0) && (!is.null(p)))
    stop("You have defined 'p' (periodicity) parameter with an incorrect value.")
  if ((tip != "M") && (!is.na(tip)))
    stop("You have defined 'tip' parameter with an incorrect value.")

  # Split query
  df_queries <- NULL
  queries <- strsplit(query, split = "AND")
  for (queries_splited in queries) {
    var_val <- strsplit(queries_splited, split = "=")
    for (string in var_val) {
      df_queries <- rbind(df_queries, data.frame(string))
    }
  }

  # Get variables and values id
  variables <- get_variables_operation(code)
  result <- NULL
  count <- 0
  g <- 1
  for (qvalue in df_queries$string) {
    qvalue <- trimws(as.character(qvalue))
    if (count %% 2 == 0) { # variables
      variableId <- variables[match(qvalue, variables[["Nombre"]]),][["Id"]]
      variable <- paste0("g", g, "=", variableId, ":")
      result <- rbind(result, variable)
      g <- g + 1
    # values
    } else {
      value <- get_values_variableoperation(variableId, code)
      value <- value[match(qvalue, value[["Nombre"]]),][["Id"]]
      if (is.na(value)) {
        value <- ""
      }
      value <- paste0(value, "&")
      result <- rbind(result, value)
    }
    count <- count + 1
  }

  # Join query
  urlStr <- paste0(result, collapse = "")

  # Build URL
  if (ioe) {
    if (is.null(p)) {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/IOE", code, "?", urlStr, "tip=", tip, "&det=", det)
    } else {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/IOE", code, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det)
    }
  } else {
    if (is.null(p)) {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/", code, "?", urlStr, "tip=", tip, "&det=", det)
    } else {
      url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIE_METADATAOPERACION/", code, "?", urlStr, "p=", p, "&tip=", tip, "&det=", det)
    }
  }

  return(fromJSON(url))

}
