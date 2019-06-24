# API INE (Valores)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


#' @title Get values
#' @description This function returns values from a variable or variable operation
#' @param code (int) variable indentificator
#' @param operation (string) operation indentificator
#' @param resource (string) resource to access, by default \code{resource = "metadata"} to get serie metadata.
#'  Possible values are \code{all or variable_operation}
#' @param help (boolean) type any value for \code{resource} param and type \code{help = TRUE} to see params available for this \code{resource}.
#' @param det (int) \code{det = 1} to see the detail of the variable to which it belongs, \code{det = 0} by default
#' @param ioe (boolean) \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang (string) language used to obtain information
#' @examples
#' get_values(115)
#' get_values(resource = "all", help = TRUE)
#' get_values(762, operation = "IPC", resource = "variable_operation")
#' @export
get_values <- function(code = NULL, resource = "all", operation = NULL, help = FALSE, det = 0, ioe = FALSE, lang = "ES") {

  content <- NULL

  switch(resource,
    all = {
      # Help
      if (help) {
        params <- c("code (variable id)", "det", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_values(115)'))
        message(paste0('Example (extended): get_values(115, resource = "all", det = 0, lang = "ES")'))
      } else {
        content <- get_values_all(code, det, lang)
      }
    },
    variable_operation = {
      # Help
      if (help) {
        params <- c("code (variable id)", "operation (operation id)", "det", "ioe", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_values(762, operation = "IPC", resource = "variable_operation")'))
        message(paste0('Example (extended): get_values(762, operation = "IPC", resource = "variable_operation, det = 0, ioe = FALSE, lang = "ES")'))
      } else {
        content <- get_values_variableoperation(code, operation, det, ioe, lang)
      }
    },
    {
      stop('ERROR: Possible values of param "resource" are: all or variable_operation')
    }
  )

  if (!help) {
    return(content)
  }

}


# Get values (all)
# How to call: get_values(115, resource = "all")
# Examples:
# get_values_all(115)
# get_values_all(115, 1)
get_values_all <- function(id, det = 0, lang = "ES") {

  # Check det param
  if ((det < 0) || (det > 1)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLE/", id, "?det=", det)

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}


# Get values from variable operation
# How to call: get_values(762, operation = "IPC", resource = "variable_operation")
# Examples:
# get_values_variableoperation(762, 25)
# get_values_variableoperation(762, 25, 1)
# get_values_variableoperation(762, 30138, ioe = TRUE)
# get_values_variableoperation(762, 30138, ioe = TRUE, 1)
get_values_variableoperation <- function(id, op, det = 0, ioe = FALSE, lang = "ES") {

  # Check det param
  if ((det < 0) || (det > 1)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  # Build URL
  if (ioe) {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLEOPERACION/", id, "/IOE", op, "?det=", det)
  } else {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VALORES_VARIABLEOPERACION/", id, "/", op, "?det=", det)
  }

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}
