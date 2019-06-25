# API INE (Publicaciones)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


#' @title Get publications
#' @description This function returns all available publications from an operation or a date
#' @param code (string) operation or publication indentificator
#' @param resource (string) resource to access, by default \code{resource = "all"} to get serie metadata.
#'  Possible values are \code{all, operation or date}
#' @param help (boolean) type any value for \code{resource} param and type \code{help = TRUE} to see params available for this \code{resource}.
#' @param det (int) \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param ioe (boolean) \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang (string) language used to obtain information
#' @examples
#' get_publications()
#' get_publications(resource = "all", help = TRUE)
#' get_publications("IPC", resource = "operation")
#' get_publications(8, resource = "date")
#' @export
get_publications <- function(code = NULL, resource = "all", help = FALSE, det = 0, ioe = FALSE, lang = "ES") {

  content <- NULL

  switch(resource,
    all = {
      # Help
      if (help) {
        params <- c("det", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_publications()'))
        message(paste0('Example (extended): get_publications(resource = "all", det = 0, lang = "ES")'))
      } else {
        content <- get_publications_all(det, lang)
      }
    },
    operation = {
      # Help
      if (help) {
        params <- c("code (operation id)", "ioe", "det", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_publications("IPC", resource = "operation")'))
        message(paste0('Example (extended): get_publications("IPC", resource = "operation", ioe = FALSE, det = 0, lang = "ES")'))
      } else {
        content <- get_publications_operation(code, ioe, det, lang)
      }
    },
    date = {
      # Help
      if (help) {
        params <- c("code (publication id)", "det", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_publications(8, resource = "date")'))
        message(paste0('Example (extended): get_publications(8, resource = "date", det = 0, lang = "ES")'))
      } else {
        content <- get_publications_date(code, det, lang)
      }
    },
    {
      stop('ERROR: Possible values of param "resource" are: all, operation or date')
    }
  )

  if (!help) {
    return(content)
  }

}


# Get publications
# How to call: get_publications(resource = "all")
# Examples:
# get_publications_all()
get_publications_all <- function(det = 0, lang = "ES") {

  # Check det param
  if ((det < 0) || (det > 2)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES?det=", det)

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}


# Get publications operation
# How to call: get_publications("IPC", resource = "operation")
# Examples:
# get_publications_operation(25)
# get_publications_operation(30138, ioe = TRUE)
get_publications_operation <- function(code, ioe = FALSE, det = 0, lang = "ES") {

  # Check det param
  if ((det < 0) || (det > 2)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  # Build URL
  if (ioe) {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES_OPERACION/IOE", code, "?det=", det)
  } else {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONES_OPERACION/", code, "?det=", det)
  }

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}


# Get publications date
# How to call: get_publications(8, resource = "date")
# Examples:
# get_publications_date(8)
get_publications_date <- function(code, det = 0, lang = "ES") {

  # Check det param
  if ((det < 0) || (det > 2)) {
    stop("You have defined 'det' parameter with an incorrect value.")
  }

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/PUBLICACIONFECHA_PUBLICACION/", code, "?det=", det)

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}
