# API INE (Metadata crossing)
# Author: Andres Nacimiento Garcia <andresnacimiento[at]gmail[dot]com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal[at]ull[dot]es>


#' @title Get metadata crossing
#' @description This function returns data or metadata by metadata crossing
#' @param code (string) operation identificator
#' @param resource (string) resource to access, by default \code{resource = "metadata"} to get serie metadata.
#'  Possible values are \code{series or data}
#' @param help (boolean) type any value for \code{resource} param and type \code{help = TRUE} to see params available for this \code{resource}.
#' @param query (string) string separated by \code{AND} with syntax \code{variable = value} using natural language
#' @param p (int) periodicity, \code{p = 1} by default
#' @param det (int) \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param tip (string) \code{tip = M} to obtain the metadata (crossing variables-values) of the series
#' @param nlast last \code{n} values
#' @param lang (string) language used to obtain information
#' @examples
#' get_metadata_crossing()
#' get_metadata_crossing(resource = "series", help = TRUE)
#' get_metadata_crossing("IPC", resource = "series", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL")
#' get_metadata_crossing("IPC", resource = "data", query = "Provincias = Madrid AND Tipo de dato = Variacion mensual AND Grupos ECOICOP = NULL", nlast = 5)
#' @export
get_metadata_crossing <- function(code = NULL, resource = "series", help = FALSE, query = NULL, p = 1, det = 0, tip = NULL, nlast = 1, lang = "ES") {

  content <- NULL

  switch(resource,
    # Get series by metadata crossing
    series = {
      # Help
      if (help) {
        params <- c("code (operation id)", "query", "p", "det", "tip", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_metadata_crossing("IPC", resource = "series", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL")'))
        message(paste0('Example (extended): get_metadata_crossing(code = "IPC", resource = "series", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL", p = 1, det = 2, tip = "M", lang = "ES")'))
      } else {
        content <- get_series_metadataoperation(code, query, p, det, tip, lang)
      }
    },
    data = {
      if (help) {
        params <- c("code (operation id)", "query", "p", "nlast", "det", "tip", "lang")
        message(paste0('Available params for resource = ', '"', resource, '"', ' are: '))
        message(paste0("- ", params, "\n"))
        message(paste0('Example (basic): get_metadata_crossing("IPC", resource = "data", query = "Provincias = Madrid AND Tipo de dato = Variacion mensual AND Grupos ECOICOP = NULL", nlast = 5)'))
        message(paste0('Example (extended): get_metadata_crossing(code = "IPC", resource = "data", query = "Provincias = Madrid AND Tipo de dato = Variación mensual AND Grupos ECOICOP = NULL", p = 1, nlast = 5, det = 2, tip = "AM", lang = "ES")'))
      } else {
        content <- get_data_metadataoperation(code, query, p, nlast, det, tip, lang)
      }
    },
    {
     stop('ERROR: Possible values of param "resource" are: series or data')
    }
  )

  if (!help) {
    return(content)
  }
}
