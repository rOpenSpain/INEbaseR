# API INE (Series)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


#' @title Get tables
#' @description This function returns data / metadata from tables
#' @param code operation (string/int) or table (int) identificator
#' @param resource (string) resource to access, by default \code{resource = "operation"} to get tables of an operation.
#'  Possible values are \code{operation, group, group_values or data}
#' @param help (boolean) type any value for \code{resource} param and type \code{help = TRUE} to see params available for this \code{resource}
#' @param grp (int) group identification
#' @param geo (int) use \code{geo = 1} to access only tables with geographic content, \code{geo = 0} by default
#' @param nlast last \code{n} values
#' @param det (int) use \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip (string) use \code{tip = "A"} to view as friendly, specifically the view the field \code{ultima_modificacion} or use \code{tip = "AM"}
#'  to obtain the metadata (crossing variables-values) of the series and a friendly output.
#' @param ioe (boolean) use \code{ioe = TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang (string) language used to obtain information
#' @examples
#' get_tables("IPC")
#' get_tables(resource = "operation", help = TRUE)
#' get_tables(25, resource = "operation")
#' get_tables(22350, resource = "group")
#' get_tables(22350, grp = 81497, resource = "group_values")
#' get_tables(22350, nlast = 5, resource = "data")
#' @export
get_tables <- function(code = NULL, resource = "operation", help = FALSE, grp = NULL, geo = 0, nlast = 0, det = 0, tip = NULL, ioe = FALSE, lang = "ES") {

  content <- NULL

  switch(resource,
    operation = {
      # Help
      if (help) {
        params <- c("code (operation id)", "det", "geo", "tip", "ioe", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_tables("IPC")'))
        message(paste0('Example (extended): get_tables(code = "IPC", resource = "operation", det = 0, geo = 0, tip = NULL, ioe = FALSE, lang = "ES")'))
      } else {
        content <- get_tables_operation(code, det, geo, tip, ioe, lang)
      }
    },
    group = {
      # Help
      if (help) {
        params <- c("code (table id)", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_tables(22350, resource = "group")'))
        message(paste0('Example (extended): get_tables(code = 22350, resource = "group", lang = "ES")'))
      } else {
        content <- get_tables_group(code, lang)
      }
    },
    group_values = {
      # Help
      if (help) {
        params <- c("code (table id)", "grp", "det", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_tables(22350, grp = 81497, resource = "group_values")'))
        message(paste0('Example (extended): get_tables(code = 22350, grp = 81497, resource = "group_values", det = 0, lang = "ES")'))
      } else {
        content <- get_tables_group_values(code, grp, det, lang)
      }
    },
    data = {
      # Help
      if (help) {
        params <- c("code (table id)", "nlast", "det", "tip", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_tables(22350, nlast = 5, resource = "data")'))
        message(paste0('Example (extended): get_tables(22350, nlast = 5, resource = "data", det = 0, tip = NULL, lang = "ES")'))
      } else {
        content <- get_data_table(code, nlast, det, tip, lang)
      }
    },
    {
      stop('ERROR: Possible values of param "resource" are: operation, group, group_values or data')
    }
  )

  if (!help) {
    return(content)
  }

}


# Get tables operation
# Examples
# get_tables_operation(25)
# get_tables_operation(25, 2, 1, tip = "A")
# get_tables_operation(30138, ioe = TRUE, 2, 1, tip = "A")
get_tables_operation <- function(code, det = 0, geo = 0, tip = NULL, ioe = FALSE, lang = "ES") {

  # Check params
  if ((det < 0) || (det > 2)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  if ((geo < 0) || (geo > 1)) {
    stop("You have defined 'geo' parameter with an incorrect value.")
  }

  if ((tip != "A") && (!is.null(tip))) {
    stop("You have defined 'tip' parameter with an incorrect value.")
  }

  # Build URL
  if (ioe) {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/TABLAS_OPERACION/IOE", code, "?det=", det, "&geo=", geo, "&tip=", tip)
  } else {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/TABLAS_OPERACION/", code, "?det=", det, "&geo=", geo, "&tip=", tip)
  }

  # Get content
  content <- fromJSON(url)

  return(content)

}


# Get tables group
# Examples
# get_tables_group(22350)
get_tables_group <- function(id, lang = "ES") {

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/GRUPOS_TABLA/", id)

  # Get content
  content <- fromJSON(url)

  return(content)

}


# Get tables group values
# Examples
# get_tables_group_values(22350, 81497)
# get_tables_group_values(22350, 81497, 1)
get_tables_group_values <- function(id, grp, det = 0, lang = "ES") {

  # Check params
  if ((det < 0) || (det > 1)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_GRUPOSTABLA/", id, "/", grp, "?det=", det)

  # Get content
  content <- fromJSON(url)

  return(content)

}
