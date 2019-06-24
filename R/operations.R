# API INE (Operations)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


#' @title Get operations
#' @description This function returns information about operations
#' @param code (string) operation identificator
#' @param resource (string) resource to access, by default \code{resource = "metadata"} to get serie metadata.
#'  Possible values are \code{all, metadata, variables_values or by_granularity}
#' @param help (boolean) type any value for \code{resource} param and type \code{help = TRUE} to see params available for this \code{resource}.
#' @param ioe (boolean) \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param geographical_granularity (string) geographical granularity
#' @param temporal_granularity (string) temporal granularity
#' @param verbose (boolean) show more information during the process
#' @param lang (string) language used to obtain information
#' @examples
#' get_operations()
#' get_operations(resource = "all")
#' get_operations(resource = "all", help = TRUE)
#' get_operations("IPC", resource = "metadata")
#' get_operations("IPC", resource = "variables_values")
#' get_operations(resource = "by_granularity", geographical_granularity = "PROV")
#' @export
get_operations <- function(code = NULL, resource = "all", help = FALSE, ioe = FALSE, geographical_granularity = NULL, temporal_granularity = NULL, verbose = TRUE, lang = "ES") {

  content <- NULL

  switch(resource,
    all = {
      # Help
      if (help) {
        params <- c("lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_operations()'))
        message(paste0('Example (extended): get_operations(resource = "all", lang = "ES")'))
      } else {
        content <- get_operations_all(lang)
      }
    },
    metadata = {
      # Help
      if (help) {
        params <- c("code (operation id)", "ioe", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_operations("IPC", resource = "metadata")'))
        message(paste0('Example (extended): get_operations("IPC", resource = "metadata", ioe = FALSE, lang = "ES")'))
      } else {
        content <- get_operation(code, ioe, lang)
      }
    },
    variables_values = {
      # Help
      if (help) {
        params <- c("code (operation id)", "ioe", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_operations("IPC", resource = "variables_values")'))
        message(paste0('Example (extended): get_operations("IPC", resource = "variables_values", ioe = FALSE, lang = "ES")'))
      } else {
        content <- get_var_values_operation(code, ioe, lang)
      }
    },
    by_granularity = {
      # Help
      if (help) {
        params <- c("geographical_granularity", "temporal_granularity", "verbose")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_operations(resource = "by_granularity", geographical_granularity = "PROV")'))
        message(paste0('Example (extended): get_operations(resource = "by_granularity", geographical_granularity = "PROV", temporal_granularity = NULL, verbose = TRUE)'))
      } else {
        content <- get_operations_by_granularity(geographical_granularity, temporal_granularity, verbose)
      }
    },
    {
      stop('ERROR: Possible values of param "resource" are: all, metadata, variables_values or by_granularity')
    }
  )

  if (!help) {
    return(content)
  }

}


# Get operations (all)
# Examples:
# get_operations_all()
get_operations_all <- function(lang = "ES") {

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACIONES_DISPONIBLES")

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)

}

# Get operation
# Examples:
# get_operation(operation = 25)
# get_operation(operation = "IPC")
# get_operation(operation = 30138, ioe = TRUE)
get_operation <- function(operation, ioe = FALSE, lang = "ES") {

  # Build URL
  if (ioe) {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACION/IOE", operation)
  } else {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/OPERACION/", operation)
  }

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)
}
