# API INE (cache)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

get_cache_file_name <- function(data_type, code, path = "cache", sys_date = Sys.Date()) {
  file_name <- paste0(path, "/", data_type, "-", code, "_", sys_date, ".RData")
  return(file_name)
}

build_cache_directory <- function(path = "cache") {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0755")
  }
}

check_cache <- function(data_type, code){
  file_name <- get_cache_file_name(data_type, code)
  file_test("-f", file_name)
}

# Clean out of date cache files (see expiration_date parameter)
# Example: update_cache("SERIE", "IPC206449", expiration_date = 2)
update_cache <- function(data_type, code, path = "cache", expiration_date = 7){

  if (!dir.exists(path)) {
    build_cache_directory()
  } else {

    # Get all files in cache dir
    files <- list.files(path = path, all.files = TRUE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
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
  update_cache(data_type, code)

  # Save data into cache
  file_name <- get_cache_file_name(data_type, code)
  content <- data
  save(content, file = file_name, compress = TRUE, compression_level = 6)
  save.image()

}

clean_cache <- function(data_type, code, sys_date = Sys.Date()){
  file_name <- get_cache_file_name(data_type, code, sys_date = sys_date)
  file.remove(file_name)
}

get_cache <- function(data_type, code){

  # Clean out of date cache files
  update_cache(data_type, code)

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
