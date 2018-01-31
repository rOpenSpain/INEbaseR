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
  return(fromJSON(paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES")))
}

#' @title Get variable operation
#' @description This function returns a data frame with system variables of an operation from an id or code
#' @param code operation identification
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_variables_operation(25)
#' get_variables_operation("IPC")
#' get_variables_operation(30138, ioe = TRUE)
#' @export
get_variables_operation <- function(code, ioe = FALSE, lang = "ES") {
  if (ioe)
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES_OPERACION/IOE", code)
  else
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/VARIABLES_OPERACION/", code)
  return(fromJSON(url))
}

#' @title Get variable values operation
#' @description This function returns a data frame with all values and variables of an operation from an id or code
#' @param op operation identification
#' @param ioe \code{TRUE} if code is in format \code{IO30138}, and \code{FALSE} by default
#' @param lang language used to obtain information
#' @details
#' Numeric code \code{id}
#' Alphabetic code \code{IPC}
#' \code{IOE} code (Inventario de Operaciones Estadísticas)
#' @examples
#' get_var_values_operation(25)
#' get_var_values_operation("IPC")
#' get_var_values_operation(30138, ioe = TRUE)
#' @export
get_var_values_operation <- function(op, ioe = FALSE, lang = "ES") {
  var_val_op <- NULL
  var_val_op_name_variable <- NULL
  variables <- get_variables_operation(op, ioe, lang)
  count <- 1

  for (id in variables$Id){
    var_val_op <- rbind(var_val_op, get_values_variableoperation(id, op, det = 0, ioe, lang))
    for (index in count:length(var_val_op$Id)){
      var_val_op_name_variable <- rbind(var_val_op_name_variable, variables[match(id, variables[["Id"]]),][["Nombre"]])
      count <- count + 1
    }
  }

  var_val_op$`Nombre variable` <- var_val_op_name_variable
  var_val_op$Operacion <- op
  return(var_val_op[,c(1,6,2,5:3)])
}
