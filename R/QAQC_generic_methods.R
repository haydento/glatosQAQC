#' @include classes.R

# Functions that identify suspect values obtained from extracted detection data
# this is not exported.

# create a QAQC generic function with multiple methods that applies QAQC checks depending on input class

# Generic function that performs QAQC checks.
QAQC <- S7::new_generic("QAQC", dispatch_args = "x")

# QAQC method for summary table output produced for table in shiny app (tableize_class)
S7::method(QAQC, tableize_class) <- function(x){

  out <- x@table

  # format time columns
  #  out[, `comp download` := format(`comp download`, "%Y-%m-%d %H:%M:%S")]
  # out[, `int tag init` := format(`int tag init`, "%Y-%m-%d %H:%M:%S")]
  
  # prepare table to make html report
  # change color of last detection when last detection is
  # not on the same day as download time
  # SOP is to test receiver immediately prior to download
  down_last_dtc_days <- as.Date(out$`rec download`) - as.Date(out$`last det`)
  
  # color last det and receiver download time red when last det is not on same day as receiver download or when there is a NA in last detection or when last det is NA
  # this check is based on SOP that says all receivers should be tested with sync tag immediately prior to download.
  out$`last det` <- kableExtra::cell_spec(out$`last det`, "html", 
    color = ifelse(down_last_dtc_days != 0 # changed from != for testing
      | is.na(down_last_dtc_days) 
      | is.na(out$`last det`), "red", "black"))

  out$`rec download` <- kableExtra::cell_spec(out$`rec download`, "html", 
    color = ifelse(down_last_dtc_days != 0 # changed from != for testing
      | is.na(down_last_dtc_days), "red", "black"))

  # color first det and receiver initialize time red when first det and receiver initialize time are not on the same day.
  # this check is based on SOP that says all receivers should be tested with syn tag immediately after initialization.
  # also first det is colored red if first det is NA
  init_days <- as.Date(out$`first det`) - as.Date(out$`rec init`) 

  out$`first det` <- kableExtra::cell_spec(out$`first det`, "html", color = ifelse(init_days != 0 | is.na(init_days) | is.na(out$`first det`), "red", "black"))
  out$`rec init` <- kableExtra::cell_spec(out$`rec init`, "html", color = ifelse(init_days != 0 | is.na(init_days), "red", "black"))

  # change text to red when number of detections is 0
  out$`num det` <- kableExtra::cell_spec(out$`num det`, "html", color = ifelse(out$`num det` == 0, "red", "black"))

  # change text of `mem avail` to red when memory available is <10%
  out$`mem avail` <- kableExtra::cell_spec(out$`mem avail`, "html", color = ifelse(out$`mem avail` < 10, "red", "black"))

  return(out)
  
}

# QAQC method for clock comparisons (class clk)
S7::method(QAQC, clk) <- function(x){

  x <- data.table(field = c(
    "Data recorder",
    "Study code",
    "Time zone",
    "Full test tag ID",
    "Vue version",
    "NIST time (lcl)",
    "computer time (lcl)",
    "time difference (s)",
    "vdat version"), 
    value = c(
      x@recorder,
      x@code,
      x@tz,
      x@tag,
      x@vue,
      format(x@tsync$`NIST time (lcl)`),
      format(x@tsync$`computer time (lcl)`),
      x@tsync$`time difference (s)`,
      x@vdat_ver)
    )

  # highlight if computer and NIST time is more than 2 seconds apart.
  
  if(as.numeric(x[field == "time difference (s)",]$value) > 2){
    
    x[field == "time difference (s)",]$value <- kableExtra::cell_spec(x[field == "time difference (s)",]$value, "html", color = "red")
    x[field == "time difference (s)",]$field <- kableExtra::cell_spec(x[field == "time difference (s)",]$field, "html", color = "red")
  }  
  return(x)
}

# QAQC method for highlighting the same issues in excel file output
S7::method(QAQC, excel_class) <- function(x, output){

  down_last_dtc_days = `rec download` = `last det` = init_days = `first det` = `rec init` = `num det` = `mem avail` = v = NULL # due to NSE notes in R CMD check

  # bring in worksheets that need colored (detections and metadata)
  meta <- x@lst$Metadata@table
  out <- copy(x@lst$Receivers@table)
  base_out = copy(x@lst$Receivers@table)
  
  # color 'last det' and 'receiver download time' when last detection is not of same day as receiver download or when there is a NA in last detection or when 'last det' is NA
  out[, down_last_dtc_days := as.Date(`rec download`) - as.Date(`last det`)]
  out[, `last det` := fifelse(down_last_dtc_days != 0 | 
                                is.na(down_last_dtc_days) | 
                                is.na(out$`last det`), 1, 0)] # changed from != to == for testing
  out[, `rec download` := fifelse(down_last_dtc_days != 0 | 
                                    is.na(down_last_dtc_days) | 
                                    is.na(out$`last det`), 1, 0)]
  #set(out, j = out$`last det`, value = as.numeric(out$`last det`))


  # color `first det` and `receiver initialize time` red when `first det` and `receiver initialize time` are not on the same day.
  # this check is based on SOP that says all receivers should be tested with syn tag immediately after initialization.
  # also first det is colored red if `first det` is NA
  out[, init_days := as.Date(out$`first det`) - as.Date(out$`rec init`)]
  out[, `first det` := fifelse(init_days != 0 | is.na(init_days) | is.na(out$`first det`), 1, 0)] 
  out[, `rec init` := fifelse(init_days != 0 | is.na(init_days), 1, 0)]

  # change text to red when number of detections is 0
  out[, `num det` := fifelse(out$`num det` == 0, 1, 0)]

  # change text of `mem avail` to red when memory available is <10%
  out[, `mem avail` := fifelse(out$`mem avail` < 10, 1, 0)]

  # delete temporary columns
  out[,`:=` (`init_days` = NULL, `down_last_dtc_days` = NULL)] 


  # columns marked in output
  cols_colored <- c("last det", "rec download", "first det", "rec init", "num det", "mem avail")
  cols_no_color <- setdiff(names(out), cols_colored)

  out[, (cols_no_color) := NA]

  out <- data.table(r = c(t(row(out))), col = c(t(col(out))), v = c(t(out)))
  out <- out[v == 1,]
  out <- unique(out, by = c("r", "col"))
  row_idx <-  out[["r"]] + 1
  col_idx <- out[["col"]]

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "detections")
  openxlsx::addWorksheet(wb, sheetName = "metadata")
  red_style <- openxlsx::createStyle(fontColour = rgb(1,0,0),
                                     textDecoration = "italic",
                                     )
  
  openxlsx::addStyle(wb, sheet = "detections",
                     style = red_style,
                     rows = row_idx,
                     cols = col_idx,
                     stack = TRUE)

  # color time difference > 2 seconds red
  if(as.numeric(meta[field == "time difference (s)",]$value) > 2){
    openxlsx::addStyle(wb, sheet = "metadata",
                       style = red_style,
                       rows = 8,
                       cols = c(1,2),
                       stack = TRUE,
                       gridExpand = TRUE)
  }
  
  openxlsx::writeData(wb, sheet = "detections", x = base_out)
  openxlsx::writeData(wb, sheet = "metadata", x = meta)
  openxlsx::saveWorkbook(wb, file = output, overwrite = TRUE, return = TRUE)

}

                      




