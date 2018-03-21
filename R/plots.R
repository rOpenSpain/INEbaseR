# API INE (Plots)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

plot_detect_date_pattern <- function(timestamp_vector) {

  # search in vector of time stamp
  for (i in 1:length(timestamp_vector)){

    # GET A PATTERN
    month <- as.numeric(format(timestamp_vector[i], "%m")) ## get numeric month

    # if is not out of vector
    if (i + 1 <= length(timestamp_vector)) {
      if (month == 12){
        nextmonth <- as.numeric(format(timestamp_vector[i+1], "%m"))+12 # if next month is 1 (january) and this month is 12 (december)
      }else{
        nextmonth <- as.numeric(format(timestamp_vector[i+1], "%m"))
      }
    }

    # latest date of vector
    else{
      if (month == 12){
        month <- as.numeric(format(timestamp_vector[i-1], "%m"))
        nextmonth <- as.numeric(format(timestamp_vector[i], "%m"))
      }else{
        month <- as.numeric(format(timestamp_vector[i-1], "%m"))
        nextmonth <- as.numeric(format(timestamp_vector[i], "%m"))+12 # if next month is 1 (january) and this month is 12 (december)
      }
    }

    pattern <- nextmonth - month # pattern

    # GET IF THE PATTERN IS ALWAYS THE SAME
    if (i != 1) {
      if (pattern == pattern_prev){
        pattern_prev <- pattern # save previous pattern
      }
      else {
        return(-1) # if there isn't the same pattern in all dates
      }
    }
    # first date pattern of vector
    else {
      pattern_prev <- pattern # in first vector element: previous pattern = pattern
    }
  }

  # the pattern is returned
  return(pattern)

}


#' @title Plot series
#' @description This function draws a plot with data of a series from an id and/or from a date or date range
#' @param code identification code of a serie (e.g. "IPC206449")
#' @param date_start start date in format (string) \code{YYYY-MM-DD}
#' @param date_end end date in format (string) \code{YYYY-MM-DD}
#' @param nult last \code{n} values
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param type what type of plot should be drawn, \code{type = "p"} (for points) by default. See \code{type} in \code{\link{plot}}
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
  timestamp_vector <- as.POSIXct((data$Fecha+0.1)/1000, origin = "1960-01-01", tz = "GMT")
  plot_detect_date_pattern(timestamp_vector)
  plot(x = timestamp_vector, y = data$Valor, xlab = "", ylab = "", type = type)
  title(main = paste("Datos de la serie", code), xlab = "Fechas", ylab = "Valores de la serie")
}

