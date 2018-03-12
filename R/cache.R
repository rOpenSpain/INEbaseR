# API INE (cache)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

check_cache <- function(data_type, code){
  file_name <- paste0("cache/", data_type, "_", code, ".RData")
  file_test("-f", file_name)
}

save_cache <- function(url, data_type, code){
  file_name <- paste0("cache/", data_type, "_", code, ".RData")
  content <- fromJSON(url)
  save(content, file = file_name, compress = TRUE, compression_level = 6)
  save.image()
}

get_cache <- function(data_type, code){
  file_name <- paste0("cache/", data_type, "_", code, ".RData")
  load(file_name)
  return(content)
}

clean_cache <- function(data_type, code){
  file_name <- paste0("cache/", data_type, "_", code, ".RData")
  file.remove(file_name)
}
