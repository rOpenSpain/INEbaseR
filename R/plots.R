# API INE (Plots)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

plot_detect_date_pattern <- function(timestamp_vector) {

  # get periodicity

  pattern <- c()
  # search in vector of time stamp
  for (i in 1:(length(timestamp_vector)-1)){

    # GET A PATTERN
    month <- as.numeric(format(timestamp_vector[i], "%m")) ## get numeric month

    # if is not out of vector
    if (i + 1 <= length(timestamp_vector)) {
      nextmonth <- as.numeric(format(timestamp_vector[i+1], "%m"))
      if (nextmonth-month <= 0){
        nextmonth <- as.numeric(format(timestamp_vector[i+1], "%m"))+12 # if next month is 1 (january) and this month is 12 (december)
      }
    }

    pattern <- c(pattern, nextmonth - month) # pattern vector

  }

  # check if the pattern values are all equals
  if(ifelse(length(unique(pattern)) == 1, TRUE, FALSE)) {
    pattern <- pattern[1]
  }
  else {
    pattern <- -1
  }

  # the pattern is returned
  return(pattern)

}

#' @title Get frequency
#' @description This function return the periodicity (number) of a time serie
#' @param periodicity string with a periodicity
#' @examples
#' get_frequency("Mensual")
#' get_frequency("Anual")
#' @export
get_frequency <- function(periodicity) {

  switch(periodicity,
    "Mensual" = {
      frequency <- 12
    },
    "Anual" = {
      frequency <- 1
    },
    {
      frequency <- 0
    }
  )

  return(frequency)

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
  serie <- get_serie(code)
  if (is.na(type))
    type = "p"
  timestamp_vector <- as.POSIXct((data$Fecha + 0.1) / 1000, origin = "1970-01-01", tz = "CET")
  #plot_detect_date_pattern(timestamp_vector)
  # obtener T3 preiodicidad con get_serie("IPC206449")$T3_Periodicidad
  plot(x = timestamp_vector, y = data$Valor, xlab = "", ylab = "", type = type)
  title(main = paste0(serie$Nombre, "\n", "Serie (", code, ")"), xlab = "Fechas", ylab = "Valores de la serie")
}

#' @title Highcharts series
#' @description This function draws a highchart with data of a series from an id and/or from a date or date range
#' @param code identification code of a serie (e.g. "IPC206449")
#' @param date_start start date in format (string) \code{YYYY-MM-DD}
#' @param date_end end date in format (string) \code{YYYY-MM-DD}
#' @param nult last \code{n} values
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' highcharts_series("IPC206449", nult = 1) # Get the latest data of a series
#' highcharts_series("IPC206449", nult = 5) # Get the \code{n} last data of a series
#' highcharts_series("IPC206449", "2013-01-01", "2016-01-01") # Get data of a series between two dates
#' highcharts_series("IPC206449", "2010-01-01") # Get data from a series from a date
#' @export
highcharts_series <- function(code, date_start = NA, date_end = NA, nult = 0, det = 0, lang = "ES") {

  data <- get_data_serie(code, date_start, date_end, nult, det, lang)$Data
  serie <- get_serie(code)
  frequency <- get_frequency(serie$T3_Periodicidad)
  data_ts <- ts(data = data$Valor, start = data$Anyo[[1]], frequency = frequency)

  # Represent time series
  highchart() %>%
    hc_title(text = serie$Nombre) %>%
    hc_subtitle(text = paste0("Serie (", code, ")")) %>%
    hc_yAxis(title = list(text = "Valores de la serie")) %>%
    hc_xAxis(type = "datetime", title = list(text = "Fechas")) %>%
    hc_add_series(data = data_ts, name = paste0("Serie ", code))
}

