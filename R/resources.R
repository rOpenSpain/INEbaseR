# API INE (Resources)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

get_content <- function(url) {

  print(url)
  # Get URL content and catch errors using tryCatch
  result <- tryCatch({
    content <- fromJSON(url)
  }, error = function(err) {
    # error handler picks up where error was generated
    print(paste("ERROR:  ",err))
    seconds <- 600
    print(paste0("Waiting ", seconds, " seconds for try it again ..."))
    Sys.sleep(600)
    get_content(url)
  }) # END tryCatch

  return(content)

}
