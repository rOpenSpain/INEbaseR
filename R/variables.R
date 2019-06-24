# API INE (Variables)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


#' @title Get variables
#' @description This function returns all or operations variables
#' @param code (string) operation identifier
#' @param resource (string) resource to access, by default \code{resource = "metadata"} to get serie metadata.
#'  Possible values are \code{all or operation}
#' @param help (boolean) type any value for \code{resource} param and type \code{help = TRUE} to see params available for this \code{resource}.
#' @param ioe (boolean) \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang (string) language used to obtain information
#' @examples
#' get_variables()
#' get_variables(resource = "all", help = TRUE)
#' get_variables("IPC", resource = "operation")
#' @export
get_variables <- function(code = NULL, resource = "all", help = FALSE, ioe = FALSE, lang = "ES") {

  content <- NULL

  switch(resource,
    all = {
      # Help
      if (help) {
        params <- c("lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_variables()'))
        message(paste0('Example (extended): get_variables(resource = "all", lang = "ES")'))
      } else {
        content <- get_variables_all(lang)
      }
    },
    operation = {
      # Help
      if (help) {
        params <- c("code (operation id)", "ioe", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_variables("IPC", resource = "operation")'))
        message(paste0('Example (extended): get_variables("IPC", resource = "operation", ioe = FALSE, lang = "ES")'))
      } else {
        content <- get_variables_operation(code, ioe, lang)
      }
    },
    {
      stop('ERROR: Possible values of param "resource" are: all or operation')
    }
  )

  if (!help) {
    return(content)
  }

}


# Get variables (all)
# How to call:
# get_variables()
# get_variables(resources = "all")
# Examples:
# get_variables_all()
get_variables_all <- function(lang = "ES") {

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES")

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)
}


# Get variable operation
# How to call: get_variables("IPC", resource = "operation")
# Examples:
# get_variables_operation(operation = 25)
# get_variables_operation(operation = "IPC")
# get_variables_operation(operation = 30138, ioe = TRUE)
get_variables_operation <- function(operation, ioe = FALSE, lang = "ES") {

  # Build URL
  if (ioe) {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES_OPERACION/IOE", operation)
  } else {
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES_OPERACION/", operation)
  }

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)
}


# Get variable values operation
# How to call: get_operations("IPC", resource = "variables_values")
# Examples:
# get_var_values_operation(operation = 25)
# get_var_values_operation(operation = "IPC")
# get_var_values_operation(operation = 30138, ioe = TRUE)
get_var_values_operation <- function(operation, ioe = FALSE, lang = "ES") {

  # Variables
  var_val_op <- NULL
  var_val_op_name_variable <- NULL

  # Get operation variables
  variables <- get_variables(operation, resource = "operation", ioe = ioe, lang = lang)

  count <- 1
  for (id in variables$Id){
    var_val_op <- rbind(var_val_op, get_values_variableoperation(id, operation, det = 0, ioe, lang))
    for (index in count:length(var_val_op$Id)){
      var_val_op_name_variable <- rbind(var_val_op_name_variable, variables[match(id, variables[["Id"]]),][["Nombre"]])
      count <- count + 1
    }
  }

  var_val_op$`Nombre variable` <- var_val_op_name_variable
  var_val_op$Operacion <- operation

  # Order columns
  content <- var_val_op[,c(1,6,2,5:3)]

  return(content)

}
