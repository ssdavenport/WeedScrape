# functions

# This function replaces all read_html() calls
read_html_safely <- function(url, times_tried=0, max_trials=5, delay=0, quietly = T) {
  
  # Try getting page html 5 times, or output error
  tryCatch(
    expr = {
      Sys.sleep(delay)
      read_html(url)},
    error =  function(e) {
      times_tried <- times_tried + 1
      if(times_tried == max_trials) {
        if(!quietly) message(paste0("\nno html page for url: ", url))
        return(NA)
      }
      read_html_safely(url, times_tried)
    }
  )
}
