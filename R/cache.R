# API INE (cache)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

build_cache_directory <- function(path = "cache") {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0755")
  }
}

check_cache <- function(data_type, code){
  file_name <- paste0("cache/", data_type, "_", code, ".RData")
  file_test("-f", file_name)
}

build_cache <- function(data, data_type, code){

  # Build cache directory if not exists
  build_cache_directory()

  # Clean existing cache
  if (check_cache(data_type, code)){
    clean_cache(data_type, code)
  }

  # Save data into cache
  file_name <- paste0("cache/", data_type, "_", code, ".RData")
  content <- data
  save(content, file = file_name, compress = TRUE, compression_level = 6)
  save.image()

}

clean_cache <- function(data_type, code){
  file_name <- paste0("cache/", data_type, "_", code, ".RData")
  file.remove(file_name)
}

get_cache <- function(data_type, code){
  if (check_cache(data_type, code)) {
    file_name <- paste0("cache/", data_type, "_", code, ".RData")
    load(file_name)
    return(content)
  }
  else {
    stop(paste0("No cache found to ", data_type, " ", code, ". Use parameter cache = FALSE"))
  }
}
