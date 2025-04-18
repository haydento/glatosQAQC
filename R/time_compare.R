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

  # extract vdat version used to extract data
  # this is displayed in metadata output- This value is consistent for all receivers
  vdat_pth = glatosQAQC::check_vdat()  
  shell_out <- sys::exec_internal(cmd = vdat_pth, args = "--version")
  vdat_ver <- rawToChar(shell_out$stdout)
  vdat_ver <- unlist(strsplit(vdat_ver, "\r?\n"))

  out <- data.table(field = c("NIST time (lcl)", "computer time (lcl)", 
                              "time difference (s)", "vdat version"), 
                    value = c(format(tnist, tz = "America/Detroit"), 
                              format(tlocal, tz = "America/Detroit"), 
                              tsync,
                              vdat_ver))  
  
    
  return(out)
}
