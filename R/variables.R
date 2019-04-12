# API INE (Variables)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

#' @title Get variables (all)
#' @description This function returns a data frame with all system variables
#' @param lang language used to obtain information
#' @examples
#' get_variables_all()
#' @export
get_variables_all <- function(lang = "ES") {

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES")

  # Get content
  content <- get_content(url, verbose = FALSE)

  return(content)
}

#' @title Get variable operation
#' @description This function returns a data frame with system variables of an operation from an id or code
#' @param operation operation identifier
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @examples
#' get_variables_operation(operation = 25)
#' get_variables_operation(operation = "IPC")
#' get_variables_operation(operation = 30138, ioe = TRUE)
#' @export
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

#' @title Get variable values operation
#' @description This function returns a data frame with all values and variables of an operation from an id or code
#' @param operation operation identifier
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @examples
#' get_var_values_operation(operation = 25)
#' get_var_values_operation(operation = "IPC")
#' get_var_values_operation(operation = 30138, ioe = TRUE)
#' @export
get_var_values_operation <- function(operation, ioe = FALSE, lang = "ES") {

  # Variables
  var_val_op <- NULL
  var_val_op_name_variable <- NULL

  # Get operation variables
  variables <- get_variables_operation(operation, ioe, lang)

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
