##' time comparison function- pulls time information from internet (if available) and calculates difference between computer (system) time and internet. If these are different, then computer was not synchronized prior to download.
##'
##' @export

time_compare <- function(){

  # get official US time (NIST)
  tnist <- try(httr::GET("http://www.google.com/")$date, silent = TRUE)
  if (inherits(tnist, "try-error")) tnist <- NA
  
  # check time difference between local and internet server
  #get computer system time
  tlocal <- Sys.time()

  #calculate difference in seconds
  tsync <- abs(round(as.numeric(tnist) - as.numeric(tlocal), 2))
  tsync <- kableExtra::cell_spec(tsync, "html", color = ifelse(tsync >= 2 | is.na(tsync), "red", "black")) 

  out <- list(tnist = tnist, tlocal = tlocal, tsync = tsync)

  return(out)
}
