# API INE (cache)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

get_cache_file_name <- function(data_type, code, sys_date = Sys.Date()) {
  directory_root <- get_cache_directory_path()
  file_name <- paste0(directory_root, "/", data_type, "-", code, "_", sys_date, ".RData")
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
check_cache <- function(data_type, code, get_file_name = FALSE, expiration_date = 30){

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
# Example: clean_outofdate_cache("SERIEOPERATION", 4, expiration_date = 15)
clean_outofdate_cache <- function(data_type, code, expiration_date = 30){

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

clean_cache <- function(data_type = NA, code = NA, sys_date = Sys.Date(), all = FALSE){

  directory_root <- get_cache_directory_path()

  if (all) {
    files <- list.files(path = directory_root, all.files = TRUE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
    for (i in 1:length(files)) {
      file_name <- paste0(directory_root, "/", files[i])
      file.remove(file_name)
      print(paste0("Removed ", files[i]))
    }
  } else {
    file_name <- get_cache_file_name(data_type, code, sys_date = sys_date)
    file.remove(file_name)
  }
}

get_cache <- function(data_type, code){

  # Clean out of date cache files
  clean_outofdate_cache(data_type, code)
  content <- NULL

  if (check_cache(data_type, code, get_file_name = TRUE)$condition) {
    file_name <- check_cache(data_type, code, get_file_name = TRUE)$file
    load(file_name)
    return(content)
  }
  else {
    stop(paste0(
      "\nNo cache data found to: ", data_type, " ", code, ". Use parameter cache = FALSE",
      "\n\nNOTE: It is possible that this data were in cache but has been expired. By default expiration date is 30 days."
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
#' @examples
#' update_cache(code = 249)
#' update_cache(code = 249, page = 1)
#' update_cache(n = 3)
#' @export
update_cache <- function(code = 0, n = 0, page = NA, pagination = TRUE, page_start = NA, page_end = NA, benchmark = TRUE, force = FALSE) {

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

    series_operation <- get_series_operation(code, pagination = pagination, page = page, page_start = page_start, page_end = page_end, cache = FALSE)

    if (length(series_operation) == 0) {
      clean_cache("SERIEOPERATION", code)
      stop("No operations founds for code = ", code)
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
        series_operation <- get_series_operation(code = operations$Id[i], pagination = pagination, page = page, page_start = page_start, page_end = page_end, cache = FALSE)
        print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' has been cached"))

      # Update only outdated caché files
      } else {
        if (!check_cache("SERIEOPERATION", operations$Id[i])) {
          series_operation <- get_series_operation(code = operations$Id[i], pagination = pagination, page = page, page_start = page_start, page_end = page_end, cache = FALSE)
          print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' has been cached"))
        } else {
          print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' IGNORED (already is updated)"))
        }
      }

    }
  }

  # Stop the clock
  if (benchmark) {
    print(proc.time() - ptm)
  }

}
