# API INE (Plots)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

# Get frequency
# This function return the periodicity (number) of a time serie
# param periodicity: string with a periodicity
# param data: result of \code{get_data_serie()$Data} call
# Examples:
# get_frequency("Mensual")
# get_frequency("Trimestral")
# get_frequency("Anual")
get_frequency <- function(periodicity, data = NULL) {

  if (periodicity == "Mensual") {
    frequency <- 12
  }
  else if (periodicity == "Trimestral") {
    frequency <- 4
  }
  else if (periodicity == "Anual") {
    frequency <- 1
  }
  else {
    frequency <- 0
  }

  if (!is.null(data)) {

    if (frequency == 12) {
      if (length(data$FK_Periodo) == 1) {
        month <- paste0(0, data$FK_Periodo)
      } else {
        month <- data$FK_Periodo
      }
      return(paste0(data$Anyo, "/", month, "/01"))
    }

    if (frequency == 4) {
      cont <- 0
      for (i in 1:length(data$FK_Periodo)) {
        month <- as.numeric(data$FK_Periodo[i]) - 13 - (3 * 1) + 0 * i + cont - 2
        if (length(month) == 1) {
          month <- paste0(0, month)
        }
        frequency[i] <- paste0(data$Anyo[i], "/", month, "/01")
        cont <- cont + 2
        if (data$FK_Periodo[i] == 22) cont <- 0
      }
      return(frequency)
    }

    if (frequency == 1) {
      return(paste0(data$Anyo, "/01/01"))

    }

  }

  return(frequency)

}


# Plot series (private)
# How to call: represent_series("IPC206449", resource = "plot", nlast = 5, type = "l")
# Examples:
# plot_series("IPC206449", nult = 1) # Get the latest data of a series
# plot_series("IPC206449", nult = 5, type = "l") # Get the \code{n} last data of a series
# plot_series("IPC206449", "2013-01-01", "2016-01-01") # Get data of a series between two dates
# plot_series("IPC206449", "2010-01-01") # Get data from a series from a date
plot_series <- function(code, date_start = NULL, date_end = NULL, nult = 0, det = 0, type = NULL, lang = "ES") {

  if (is.null(type)) {
    type = "p"
  }

  #timestamp_vector <- as.POSIXct((data$Fecha + 0.1) / 1000, origin = "1970-01-01", tz = "CET")

  #data <- get_data_serie(code, date_start, date_end, nult, det, lang)$Data
  data <- get_series(code, resource = "data", nlast = nult, date_start = date_start, date_end = date_end, det = det, lang = lang)$Data
  #serie <- get_series(code)
  serie <- get_series(code, resource = "metadata", det = 2, tip = "M")
  frequency <- get_frequency(serie$Periodicidad$Nombre, data)
  print(frequency)

  plot(x = as.Date(frequency), y =  data$Valor, xlab = "", ylab = "", type = type)
  title(main = paste0(serie$Nombre, "\n", "Serie (", code, ")"), xlab = "Fechas", ylab = "Valores de la serie")
}


# Highcharts series
# How to call: represent_series("IPC206449", resource = "highcharts", nlast = 5)
# Examples:
# highcharts_series("IPC206449", nult = 1) # Get the latest data of a series
# highcharts_series("IPC206449", nult = 5) # Get the \code{n} last data of a series
# highcharts_series("IPC206449", "2013-01-01", "2016-01-01") # Get data of a series between two dates
# highcharts_series("IPC206449", "2010-01-01") # Get data from a series from a date
highcharts_series <- function(code, date_start = NULL, date_end = NULL, nult = 0, det = 0, lang = "ES") {

  #data <- get_data_serie(code, date_start, date_end, nult, det, lang)$Data
  data <- get_series(code, resource = "data", nlast = nult, date_start = date_start, date_end = date_end, det = det, lang = lang)$Data
  #serie <- get_series(code)
  serie <- get_series(code, resource = "metadata", det = 2, tip = "M")
  frequency <- get_frequency(serie$Periodicidad$Nombre)
  data_ts <- ts(data = data$Valor, start = data$Anyo[[1]], frequency = frequency)

  # Represent time series
  highchart() %>%
    hc_title(text = serie$Nombre) %>%
    hc_subtitle(text = paste0("Serie (", code, ")")) %>%
    hc_yAxis(title = list(text = "Valores de la serie")) %>%
    hc_xAxis(type = "datetime", title = list(text = "Fechas")) %>%
    hc_add_series(data = data_ts, name = paste0("Serie ", code))
}

