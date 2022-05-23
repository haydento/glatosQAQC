##' Complete check of extracted detection data
##' @param out extracted data from VRL as created by running a process_table function

##' @export
#out <- foo
#tst <- QAQC(out)


QAQC <- function(out){

  # format time columns
#  out[, `comp download` := format(`comp download`, "%Y-%m-%d %H:%M:%S")]
 # out[, `int tag init` := format(`int tag init`, "%Y-%m-%d %H:%M:%S")]
  
  # prepare table to make html report
  # change color of last detection when last detection in not on the same day as download time
  # SOP is to test receiver immediately prior to download
  down_last_dtc_days <- as.Date(out$`rec download`) - as.Date(out$`last det`)
  
  # color `last det` and `receiver download time` red when last det is not on same day as receiver download or when there is a NA in last detection or when `last det` is NA
  # this check is based on SOP that says all receivers should be tested with sync tag immediately prior to download.
  out$`last det` <- kableExtra::cell_spec(out$`last det`, "html", color = ifelse(down_last_dtc_days !=0 | is.na(down_last_dtc_days) | is.na(out$`last det`), "red", "black"))

  out$`rec download` <- kableExtra::cell_spec(out$`rec download`, "html", color = ifelse(down_last_dtc_days != 0 | is.na(down_last_dtc_days), "red", "black"))

  # color `first det` and `receiver initialize time` red when `first det` and `receiver initialize time` are not on the same day.
  # this check is based on SOP that says all receivers should be tested with syn tag immediately after initialization.
  # also first det is colored red if `first det` is NA
  init_days <- as.Date(out$`first det`) - as.Date(out$`rec init`) 

  out$`first det` <- kableExtra::cell_spec(out$`first det`, "html", color = ifelse(init_days != 0 | is.na(init_days) | is.na(out$`first det`), "red", "black"))
  out$`rec init` <- kableExtra::cell_spec(out$`rec init`, "html", color = ifelse(init_days != 0 | is.na(init_days), "red", "black"))

  # change text to red when number of detections is 0
  out$`num det` <- kableExtra::cell_spec(out$`num det`, "html", color = ifelse(out$`num det` == 0, "red", "black"))

  # change text of `mem avail` to red when memory available is <10%
  out$`mem avail` <- kableExtra::cell_spec(out$`mem avail`, "html", color = ifelse(out$`mem avail` < 10, "red", "black"))

 return(out)
  
}

