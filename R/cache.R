# API INE (cache)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

# Build cache file name to check
get_cache_file_name <- function(data_type, code, extension = ".rds") {
  directory_root <- get_cache_directory_path()
  file_name <- paste0(directory_root, "/", data_type, "-", code, extension)
  return(file_name)
}

# Get cache directory path
get_cache_directory_path <- function(package = "INEbaseR", path = "extdata") {
  directory_root_path <- find.package(package)
  directory_path <- paste0(directory_root_path, "/", path)
  return(directory_path)
}

# Check if file is in cache or not
# Example: check_cache("SERIEOPERATION", 4, get_file_name = TRUE)
check_cache <- function(data_type, code, get_file_name = FALSE){

  # Build cache file name to check
  file_name <- get_cache_file_name(data_type, code)

  # Check if file exists
  if (file_test("-f", file_name)) {
    if (get_file_name) {
      # Return a data frame with file name
      return(data.frame(condition = TRUE, file = file_name, stringsAsFactors = FALSE))
    } else {
      # In this case the file exists
      return(TRUE)
    }
  } else {
    # File not exists
    return(FALSE)
  }

}

#' @title Update cache
#' @description This function allow update specific or all cache data
#' @param code operation identificator
#' @param n number of operation to update starting from first operation getted from \code{get_operations_all()} function.
#' @param page \code{page = 1} to obtain data of an specific page (to use this, \code{pagination = FALSE}).
#' @param pagination \code{TRUE} to obtain data page by page and \code{FALSE} by default.
#' @param page_start \code{page_start = 1} start page range to obtain data (to use this, \code{pagination = TRUE}).
#' @param page_end \code{page_end = 2} end page range to obtain data (to use this, \code{pagination = TRUE}).
#' @param benchmark used to measure the performance of the system, \code{benchmark = FALSE} by default.
#' @param force (boolean) to force to update all cache data, \code{force = FALSE} by default.
#' @param ignore_series (int) list of operation identificators to ignore. More slow series to cache are: 16, 49, 330 and 334
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @examples
#' update_cache(code = 249)
#' update_cache(code = 249, page = 1)
#' update_cache(n = 3)
#' @export
update_cache <- function(code = 0, n = 0, page = NA, pagination = TRUE, page_start = NA, page_end = NA, benchmark = TRUE, force = FALSE, ignore_series = NULL, tip = "M", det = 2) {

  if (n < 0)
    stop("You have defined 'n' parameter with an incorrect value.")
  if (code < 0)
    stop("You have defined 'code' parameter with an incorrect value.")

  # Start the clock!
  if (benchmark) {
    rnorm(100000)
    rep(NA, 100000)
    ptm <- proc.time()
  }

  message("Note: update all cache may take much time, please be patient ...")

  # Get all operations
  operations <- get_operations_all()

  if (code > 0) {

    series_operation <- get_series_operation(code, pagination = pagination, page = page, page_start = page_start, page_end = page_end, cache = FALSE, tip = tip, det = det)

    if (length(series_operation) == 0) {
      message(paste0("No operations founds for ", operations[operations$Id == code,][["Nombre"]], " (", code, ")"))
    } else {
      print(paste0("Operation '", operations[operations$Id == code,][["Nombre"]], "(", operations[operations$Id == code,][["Id"]], ")", "' has been cached"))
    }

  } else {
    # Get number of series to be the cached
    if (n > 0) {
      if (n > nrow(operations)) {
        stop("There are only ", nrow(operations), " operations.")
      } else {
        iterations <- n
      }
    } else {
      iterations <- nrow(operations)
    }

    for (i in 1:iterations) {

      # Cache all operations
      if (force) {

        # Ignore series
        if (!is.null(ignore_series)) {

          if (operations$Id[i] %in% ignore_series) {
            print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' IGNORED (by user)"))

          } else {
            series_operation <- get_series_operation(code = operations$Id[i], pagination = pagination, page = page, page_start = page_start, page_end = page_end, cache = FALSE, tip = tip, det = det)
            print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' has been cached"))
          }

        # Don't ignore series
        } else {
          series_operation <- get_series_operation(code = operations$Id[i], pagination = pagination, page = page, page_start = page_start, page_end = page_end, cache = FALSE, tip = tip, det = det)
          print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' has been cached"))
        }

      # Update only outdated caché files
      } else {

        # Ignore series
        if (!is.null(ignore_series)) {

          if (operations$Id[i] %in% ignore_series) {
            print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' IGNORED (by user)"))
          } else {
            if (!check_cache("SERIEOPERATION", operations$Id[i])) {
              series_operation <- get_series_operation(code = operations$Id[i], pagination = pagination, page = page, page_start = page_start, page_end = page_end, cache = FALSE, tip = tip, det = det)
              print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' has been cached"))
            } else {
              print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' IGNORED (already is updated)"))
            }
          }

        # Don't ignore series
        } else {

          if (!check_cache("SERIEOPERATION", operations$Id[i])) {
            series_operation <- get_series_operation(code = operations$Id[i], pagination = pagination, page = page, page_start = page_start, page_end = page_end, cache = FALSE, tip = tip, det = det)
            print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' has been cached"))
          } else {
            print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' IGNORED (already is updated)"))
          }

        }
      }
    }
  }

  # Stop the clock
  if (benchmark) {
    print(proc.time() - ptm)
  }

}


#' @title Update series
#' @description This function allow update specific or all cache data
#' @param serie serie identificator
#' @param benchmark used to measure the performance of the system, \code{benchmark = FALSE} by default.
#' @param page \code{page = 1} to obtain data of an specific page.
#' @param tip \code{tip = M} to obtain the metadata (crossing variables-values) of the series.
#' @param det \code{det = 2} to see two levels of depth, specifically to access the \code{PubFechaAct} object, \code{det = 0} by default
#' @param lang language used to obtain information
#' @examples
#' update_series()
#' @export
update_series <- function(serie = NULL, benchmark = TRUE, page = 1, tip = "M", det = 2, lang = "ES") {

  # Start the clock!
  if (benchmark) {
    rnorm(100000)
    rep(NA, 100000)
    ptm <- proc.time()
  }

  message("Note: update all cache may take much time, please be patient ...")


  # Update all operations
  if (is.null(serie)) {

    # OPERATOINS
    operations <- get_operations_all()
    for (operation in operations$Id) {

      # Convert operation ID to operation Name
      operation_name <- operations[operations$Id == operation,]$Nombre

      message(paste0("Checking updates for operation '", operation_name, " (", operation, ")' ..."))

      # Flags
      already_updated <- FALSE

      # Variables
      data_content <- NULL

      # Get series in cache
      series <- get_series(operation, resource = "operation")

      # Check if serie is empty
      if (is.null(series)) {
        message(paste0("** Operation not found in cache"))
        next
      }

      # Get last serie in cache
      last_serie <- get_last_serie_cache(operation)

      # Get last serie in API
      last_serie_api <- get_last_serie_api(operation)

      # Check if no data in operation
      if ((length(last_serie_api) == 0) || (length(last_serie) == 0)) {
        message(paste0("** No data found in operation"))
        next
      }

      # Check if last serie in API and last serie in cache are the same
      if (last_serie == last_serie_api) {
        message(paste0("** Skipped: operation was already updated"))
        already_updated <- TRUE
        next
      } else {
        message(paste0("** It seems that it's necessary to update series of the operation"))
        already_updated <- FALSE
      }

      # Starting to update operation
      if (!already_updated) {

        # From page 1, to page XX
        page_start <- 1
        page_end <- get_serie_page(last_serie, operation, page = page, det = det, tip = tip, lang = lang)

        # If operation URL (content) is empty
        if (is.null(page_end)) {
          message(paste0("** No data found in operation"))
          next
        }

        # PAGES
        index_series <- 1
        for (page in page_start:page_end) {

          # Build URL
          url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", operation, "?page=", page, "&det=", det, "&tip=", tip)

          # Get content
          content <- get_content(url, max_iterations = 3, seconds = 30, verbose = FALSE)

          # If error (no content found) go to next operation
          if (length(content) == 0) {
            message(paste0("** No data found in operation"))
            break
          }

          # SERIES
          message(paste0("** Updating series of operation ... (page ", page, ")"))
          for (i in 1:nrow(content)) {

            if (content$COD[i] == last_serie) {
              # Stop if found the last serie stored in cache
              break
            } else {
              # Data content
              index_series <- i
              data_content$Id <- rbind(data_content$Id, content$Id[index_series])
              data_content$Operacion <- rbind(data_content$Operacion, content$Operacion$Id[index_series])
              data_content$COD <- rbind(data_content$COD, content$COD[index_series])
              data_content$Nombre <- rbind(data_content$Nombre, content$Nombre[index_series])
              data_content$Decimales <- rbind(data_content$Decimales, content$Decimales[index_series])
              if (is.null(content$Clasificacion$Nombre[index_series])) {
                data_content$Clasificacion <- rbind(data_content$Clasificacion, NA)
              } else {
                data_content$Clasificacion <- rbind(data_content$Clasificacion, content$Clasificacion$Nombre[index_series])
              }
              data_content$Unidad <- rbind(data_content$Unidad, content$Unidad$Nombre[index_series])
              data_content$Periodicidad <- rbind(data_content$Periodicidad, content$Periodicidad$Nombre[index_series])
              data_content$MetaData <- rbind(data_content$MetaData, content$MetaData[index_series])
            }

          }

        }

        if (!already_updated) {

          # Convert to data frame
          data_content <- data.frame(data_content, stringsAsFactors = FALSE)

          # Build data content
          series <- rbind(data_content, series)

          # Save
          save_to_rds(series, operation, type = "SERIEOPERATION")

        }


      }
    }
  }

  # Stop the clock
  if (benchmark) {
    print(proc.time() - ptm)
  }

}

# Save data to RDS format
# Example: save_to_rds("provincias", type = "POLYGONS")
# Example: save_to_rds("comunidades_autonomas")
# Example: save_to_rds("municipios")
save_to_rds <- function(data, object, type = "SERIEOPERATION") {

  # File name to save (RDS)
  file_name_rds <- get_rds_file_name(object, type = type)

  # Save an object to a file
  saveRDS(data, file = file_name_rds, version = 2)

  # Output messege
  message(paste0("Notification: ",  object, "' compressed successfully to RDS format."))

}

# Read data to RDS format
# Example: get_rds_file_name("provincias", type = "POLYGONS")
# Example: get_rds_file_name("comunidades_autonomas", type = "POLYGONS")
# Example: get_rds_file_name("municipios", type = "POLYGONS")
# Example: get_rds_file_name("natcodes", type = "DATATABLE")
# Example: get_rds_file_name(25, type = "SERIESOPERATION")
get_rds_file_name <- function(object, extension = ".rds", type = "SERIESOPERATION") {

  directory_root <- get_cache_directory_path()
  type <- paste0("/", type, "-")
  file_name <- paste0(directory_root, type, object, extension)

  return(file_name)

}


#' @title Get rds content (cache)
#' @description This function returns the content of a RDS file in cache
#' @param object (string) an object of the "type" option
#' @param type (string) type of content do you want to read, \code{type = "/POLYGONS-"} by default
#' @examples
#' get_cache_rds("provincias", type = "POLYGONS")
#' get_cache_rds("comunidades_autonomas", type = "POLYGONS")
#' get_cache_rds("municipios", type = "POLYGONS")
#' get_cache_rds("natcodes", type = "DATATABLE")
#' get_cache_rds(4, type = "SERIEOPERATION")
#' @export
get_cache_rds <- function(object, type = "SERIEOPERATION") {

  # File name to load (RDS)
  file_name_rds <- get_rds_file_name(object, type = type)

  # Check if file exists
  if (check_cache(type, object)) {
    # Read RDS
    content <- readRDS(file = file_name_rds)
  } else {
    content <- NULL
  }

  return(content)

}

# Get last serie operation stored in cache
# Example: get_last_serie_cache(6)
get_last_serie_cache <- function(operation) {

  # Get all series operation from cache
  series <- get_cache_rds(operation, type = "SERIEOPERATION")

  # Get last serie metadata
  last_serie_row <- head(series, 1)

  # Get last serie COD
  last_cod <- last_serie_row$COD

  # Return
  return(last_cod)

}

# Get last serie operation stored in API
# Example: get_last_serie_api(6)
get_last_serie_api <- function(operation, page = 1, tip = "M", det = 2, lang = "ES") {

  # Build URL
  url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", operation, "?page=", page, "&det=", det, "&tip=", tip)

  # Get content
  content <- get_content(url, max_iterations = 3, seconds = 30, verbose = FALSE)

  # Get last serie COD
  last_cod <- content$COD[1]

  # Return
  return(last_cod)

}

# Find the page of a serie stored in INE API
# Example: get_serie_page("IPC251541", 25)
# Example: get_serie_page("IPC251603", 25)
get_serie_page <- function(serie, operation, page = 1, det = 2, tip = "M", lang = "ES") {

  # Flag
  serie_found <- FALSE

  while(!serie_found) {

    # Build URL
    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", operation, "?page=", page, "&det=", det, "&tip=", tip)

    # Get content
    content <- get_content(url, verbose = FALSE)

    if (length(content) == 0) {
      return(NULL)
    }

    # If not found: return 0
    if (is.null(content)) {
      return(0)

    } else {

      if (serie %in% content$COD) {

        # Break the loop
        serie_found <- TRUE

      } else {
        # Next page
        # print(paste0("Serie ", serie, " not found in page ", page))
        page <- page + 1

      }
    }

  }

  return(page)

}

