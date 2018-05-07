# API INE (cache)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

get_cache_file_name <- function(data_type, code, sys_date = Sys.Date()) {
  directory_root <- get_cache_directory_path()
  file_name <- paste0(directory_root, "/", data_type, "-", code, "_", sys_date, ".RData")
  return(file_name)
}

get_cache_directory_path <- function(package = "INEbaseR", path = "cache") {
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

check_cache <- function(data_type, code){
  file_name <- get_cache_file_name(data_type, code)
  file_test("-f", file_name)
}

# Clean out of date cache files (see expiration_date parameter)
# Example: clean_outofdate_cache("SERIE", "IPC206449", expiration_date = 2)
clean_outofdate_cache <- function(data_type, code, expiration_date = 7){

  directory_root <- get_cache_directory_path()

  if (!dir.exists(directory_root)) {
    build_cache_directory()

  } else {
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
          if ((data_type == file_splited_name[[1]][1]) && (code == file_splited_name[[1]][2])) {
            clean_cache(data_type, code, sys_date = Sys.Date() - expiration_date)
          } else {
            clean_cache(data_type, code, sys_date = Sys.Date() - expiration_date)
          }
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

clean_cache <- function(data_type = NA, code = NA, sys_date = Sys.Date(), path = "cache", all = FALSE){

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

  if (check_cache(data_type, code)) {
    file_name <- get_cache_file_name(data_type, code)
    load(file_name)
    return(content)
  }
  else {
    stop(paste0(
      "\nNo cache data found to: ", data_type, " ", code, ". Use parameter cache = FALSE",
      "\n\nNOTE: It is possible that this data were in cache but has been expired. By default expiration date is 7 days."
    ))
  }
}

# Examples: update_cache()
# Examples: update_cache(code = 25)
# Examples: update_cache(code = 25, page = NULL)
# Examples: update_cache(n = 10)
update_cache <- function(code = 0, n = 0, page = 1, benchmark = TRUE) {

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
    series_operation <- get_series_operation(code = code, det = 2, tip = "M", page = page)
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
    # Cache all operations
    for (i in 1:iterations) {
      series_operation <- get_series_operation(code = operations$Id[i], det = 2, tip = "M", page = page)
      print(paste0("[", i, "] ", "Operation '", operations$Nombre[i], "(", operations$Id[i], ")", "' has been cached"))
    }
  }

  # Stop the clock
  if (benchmark) {
    print(proc.time() - ptm)
  }

}
