# API INE (cache)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

cache_update <- function(){
  print("Cache")
}

check_cache <- function(type, code){
  file_name <- paste0("cache/", type, "_", code, ".json")
  file_test("-f", file_name)
}

download_cache <- function(url, type, code){
  file_name <- paste0("cache/", type, "_", code, ".json")
  if(check_cache(type, code)){
    file.remove(file_name)
  }
  download.file(url, file_name, mode = "wb")
}
