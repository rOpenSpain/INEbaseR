# API INE (cache)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

# Add resources
source("R/resources.R")

get_cache_file_name <- function(data_type, code, sys_date = Sys.Date()) {
  directory_root <- get_cache_directory_path()
  file_name <- paste0(directory_root, "/", data_type, "-", code, "_", sys_date, ".rds")
  return(file_name)
}

get_cache_directory_path <- function(package = "INEbaseR", path = "data") {
  directory_root_path <- find.package(package)
  directory_path <- paste0(directory_root_path, "/", path)
  return(directory_path)
}

build_cache_directory <- function() {
  directory_root <- get_cache_directory_path()
  if (!dir.exists(directory_root)) {
    dir.create(directory_root, showWarnings = TRUE, recursive = FALSE, mode = "0755")
  }
}

# Example: check_cache("SERIEOPERATION", 4, get_file_name = TRUE)
check_cache <- function(data_type, code, get_file_name = FALSE, expiration_date = 120){

  for (i in 0:expiration_date) {

    file_name <- get_cache_file_name(data_type, code, sys_date = (Sys.Date() - i))

    if (file_test("-f", file_name)) {
      if (get_file_name) {
        return(data.frame(condition = TRUE, file = file_name, stringsAsFactors = FALSE))
      } else {
        return(TRUE)
      }
    }

  }

  if (get_file_name) {
    return(data.frame(condition = FALSE, file = file_name, stringsAsFactors = FALSE))
  } else {
    return(FALSE)
  }

}

# Clean out of date cache files (see expiration_date parameter)
# Example: clean_outofdate_cache("SERIEOPERATION", 4, expiration_date = 120)
clean_outofdate_cache <- function(data_type, code, expiration_date = 120){

  directory_root <- get_cache_directory_path()

  if (!dir.exists(directory_root)) {
    build_cache_directory()
  }

  # Get all files in cache dir
  files <- list.files(path = directory_root, all.files = TRUE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
  if (length(files) > 0) {
    for (i in 1:length(files)) {

      # Split data from file name
      file_splited <- strsplit(files[i], split = "_")
      file_splited_date <- strsplit(file_splited[[1]][2], split = "[.]")[[1]][1]
      file_splited_name <- strsplit(file_splited[[1]][1], split = "-") # "SERIE", "IPC206449"

      # If file cache data is out of date
      if (file_splited_date <= Sys.Date() - expiration_date) {
        code <- file_splited_name[[1]][2]
        if ((data_type == file_splited_name[[1]][1]) && (code == file_splited_name[[1]][2])) {
          clean_cache(data_type, code, sys_date = file_splited_date)
        } else {
          clean_cache(data_type, code, sys_date = file_splited_date)
        }
      }

    }
  }


}

build_cache <- function(data, data_type, code){

  # Build cache directory if not exists
  build_cache_directory()

  # Clean existing cache
  if (check_cache(data_type, code)) {
    clean_cache(data_type, code)
  }

  # Clean out of date cache files
  clean_outofdate_cache(data_type, code)

  # Save data into cache
  file_name <- get_cache_file_name(data_type, code)
  content <- data
  save(content, file = file_name, compress = TRUE, compression_level = 6)
  save.image()

}

# Example: clean_cache("SERIEOPERATION", 104)
clean_cache <- function(data_type = NA, code = NA, sys_date = Sys.Date(), all = FALSE){

  directory_root <- get_cache_directory_path()

  if (check_cache(data_type, code)) {

    if (all) {
      files <- list.files(path = directory_root, all.files = TRUE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
      for (i in 1:length(files)) {
        file_name <- paste0(directory_root, "/", files[i])
        file.remove(file_name)
        print(paste0("Removed ", files[i]))
      }
    } else {
      file_name <- check_cache(data_type, code, get_file_name = TRUE)$file
      file.remove(file_name)
    }

  }

}

# Example: get_cache("SERIEOPERATION", 4)
get_cache <- function(data_type, code){

  # Clean out of date cache files
  # clean_outofdate_cache(data_type, code)
  content <- NULL

  if (check_cache(data_type, code)) {

    file_name <- check_cache(data_type, code, get_file_name = TRUE)$file
    load(file_name)

    return(content)

  } else {

    stop(paste0(
      "\nNo cache data found to: ", data_type, " ", code, ". Use parameter cache = FALSE",
      "\n\nNOTE: It is possible that this data were in cache but has been expired. By default expiration date is 120 days."
    ))

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
      clean_cache("SERIEOPERATION", code)
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
update_series <- function(serie = NULL, benchmark = FALSE, page = 1, tip = "M", det = 2, lang = "ES") {

  message("Note: update all cache may take much time, please be patient ...")

  # Update all operations
  if (is.null(serie)) {

    # Get all operations
    operations <- get_operations_all()$Id
    for (operation in operations) {

      message(paste0("Updating operation ", operation, " ..."))
      data_content <- NULL

      series <- get_series_operation(operation)
      last_serie <- get_last_serie(operation)

      # From page 1, to page XX
      page_start <- 1
      page_end <- get_serie_page(last_serie, operation, page = page, det = det, tip = tip, lang = lang)

      for (page in page_start:page_end) {

        # Build URL
        url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", operation, "?page=", page, "&det=", det, "&tip=", tip)
        # Get content
        content <- get_content(url, max_iterations = 3, seconds = 30)

        # Build new content
        for (i in 1:nrow(content)) {

          if (content$COD[i] != last_serie) {

            data_content$Id <- rbind(data_content$Id, content$Id[i])
            data_content$Operacion <- rbind(data_content$Operacion, content$Operacion$Id[i])
            data_content$COD <- rbind(data_content$COD, content$COD[i])
            data_content$Nombre <- rbind(data_content$Nombre, content$Nombre[i])
            data_content$Decimales <- rbind(data_content$Decimales, content$Decimales[i])
            data_content$Clasificacion <- rbind(data_content$Clasificacion, content$Clasificacion$Nombre[i])
            data_content$Unidad <- rbind(data_content$Unidad, content$Unidad$Nombre[i])
            data_content$Periodicidad <- rbind(data_content$Periodicidad, content$Periodicidad$Nombre[i])
            data_content$MetaData <- rbind(data_content$MetaData, content$MetaData[i])

          }

        }

        # Convert to data frame
        data_content <- data.frame(data_content, stringsAsFactors = FALSE)

        # Build data content
        series <- rbind(series, data_content)

        # Save
        save_to_rds(series, operation, type = "SERIEOPERATION")

      }

    }
  }

}

# ------------------ RDS Cache ------------------

# Example: save_to_rds("provincias", type = "POLYGONS")
# Example: save_to_rds("comunidades_autonomas")
# Example: save_to_rds("municipios")
save_to_rds <- function(data, object, type = "SERIEOPERATION") {

  # File name to save (RDS)
  file_name_rds <- get_rds_file_name(object, type = type)

  # Save an object to a file
  saveRDS(data, file = file_name_rds)

  # Output messege
  message(paste0("Notification: ",  object, "' compressed successfully to RDS format."))

}

# Example: get_rds_file_name("provincias", type = "POLYGONS")
# Example: get_rds_file_name("comunidades_autonomas", type = "POLYGONS")
# Example: get_rds_file_name("municipios", type = "POLYGONS")
# Example: get_rds_file_name("natcodes", type = "DATATABLE")
# Example: get_rds_file_name(25, type = "SERIESOPERATION")
get_rds_file_name <- function(object, extension = ".rds", type = "SERIESOPERATION") {

  directory_root <- get_cache_directory_path(path = "data")
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

  # Read RDS
  content <- readRDS(file = file_name_rds)

  return(content)

}

# Example: get_last_serie(4)
get_last_serie <- function(operation) {

  # Get all series operation from cache
  series <- get_cache_rds(operation, type = "SERIEOPERATION")

  # Get last serie metadata
  last_serie_row <- head(series, 1)

  # Get last serie COD
  last_cod <- last_serie_row$COD

  # Return
  return(last_cod)

}

# Example: get_serie_page("IPC251541", 25)
# Example: get_serie_page("IPC251603", 25)
get_serie_page <- function(serie, operation, page = 1, det = 2, tip = "M", lang = "ES") {

  # Flag
  serie_found <- FALSE

  while(!serie_found) {

    url <- paste0("http://servicios.ine.es/wstempus/js/", lang, "/SERIES_OPERACION/", operation, "?page=", page, "&det=", det, "&tip=", tip)
    content <- fromJSON(url)

    # If not found: return 0
    if (is.null(content)) {
      return(0)

    } else {

      if (serie %in% content$COD) {

        # Break the loop
        serie_found <- TRUE

      } else {
        # Next page
        print(paste0("Serie ", serie, " not found in page ", page))
        page <- page + 1

      }
    }

  }

  return(page)

}

