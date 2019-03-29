# API INE (Resources)
#
# Author: Andres Nacimiento Garcia <andresnacimiento@gmail.com>
# Project Director: Carlos J. Perez Gonzalez <cpgonzal@ull.es>

get_content <- function(url, loop = 1, max_iterations = 10, seconds = 60) {

  print(url)
  # Get URL content and catch errors using tryCatch
  result <- tryCatch({
    content <- fromJSON(url)
  }, error = function(err) {
    # error handler picks up where error was generated
    print(paste("ERROR:  ",err))
    print(paste0("[", loop, "/", max_iterations, "] Waiting ", seconds, " seconds for try it again..."))
    Sys.sleep(seconds)
    # Try it X times
    if (loop == max_iterations) {
      return(NULL)
    } else {
      get_content(url, loop + 1, max_iterations = max_iterations, seconds = seconds)
    }
  }) # END tryCatch

  return(content)

}
