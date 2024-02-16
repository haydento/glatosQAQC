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
  #tsync <- kableExtra::cell_spec(tsync, "html", color = ifelse(tsync >= 2 | is.na(tsync), "red", "black")) 


  out <- data.table(field = c("NIST time (lcl)", "computer time (lcl)", "time difference"), value = c(format(tnist, tz = "America/Detroit"), format(tlocal, tz = "America/Detroit"), tsync))  
  
  #out <- data.table("NIST time (lcl)" = format(tnist, tz = "America/Detroit"), "computer time (lcl)" = format(tlocal, tz = "America/Detroit"), "time difference (s)" = tsync)
  
#  out <- list(tnist = tnist, tlocal = tlocal, tsync = tsync)

  return(out)
}
