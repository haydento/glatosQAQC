# Functions that identify suspect values obtained from extracted detection data

#' @param out extracted data from VRL as created by running a process_table function - output only used in HTML report
#' @details This function runs standard checks and colorcodes html output

#' @export

QAQC <- function(out){

  # format time columns
#  out[, `comp download` := format(`comp download`, "%Y-%m-%d %H:%M:%S")]
 # out[, `int tag init` := format(`int tag init`, "%Y-%m-%d %H:%M:%S")]
  
  # prepare table to make html report
  # change color of last detection when last detection in not on the same day as download time
  # SOP is to test receiver immediately prior to download
  down_last_dtc_days <- as.Date(out$`rec download`) - as.Date(out$`last det`)
  
  # color last det and receiver download time red when last det is not on same day as receiver download or when there is a NA in last detection or when last det is NA
  # this check is based on SOP that says all receivers should be tested with sync tag immediately prior to download.
  out$`last det` <- kableExtra::cell_spec(out$`last det`, "html", color = ifelse(down_last_dtc_days !=0 | is.na(down_last_dtc_days) | is.na(out$`last det`), "red", "black"))

  out$`rec download` <- kableExtra::cell_spec(out$`rec download`, "html", color = ifelse(down_last_dtc_days != 0 | is.na(down_last_dtc_days), "red", "black"))

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


#' Checks time of computer clock by extracting true time from internet
#'
#' @details identifies and colorcodes time differnce between computer and official internet time 
#' @param input time-sync data generated from function

#' @export

# format output from clock
clock_QAQC <- function(input){
  tsync <- kableExtra::cell_spec(input, "html", color = ifelse(input >= 2 | is.na(input), "red", "black")) 
  return(tsync)
}


#' Colorcodes potential errors in excel output
#'
#' @details uses R package openxlsx to identify potential errors in vrl files and colorizes them using custom formatting in excel
#' @param input output from "process table", runs checks and makes excel file
#' @param output file path for excel worksheet
#' 
#' @export

excel_QAQC <- function(input, output){
  
  # bring in a list of worksheets (detections and metadata)
  meta <- input$metadata

  out <- copy(input$detections)
  base_out = copy(input$detections)

  # color 'last det' and 'receiver download time' when last detection is not of same day as receiver download or when there is a NA in last detection or when 'last det' is NA
  out[, down_last_dtc_days := as.Date(`rec download`) - as.Date(`last det`)]
  out[, `last det` := fifelse(down_last_dtc_days !=0 | is.na(down_last_dtc_days) | is.na(out$`last det`), 1, 0)] 
  out[, `rec download` := fifelse(down_last_dtc_days !=0 | is.na(down_last_dtc_days) | is.na(out$`last det`), 1, 0)]
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
  openxlsx::writeData(wb, sheet = "detections", x = base_out)
  openxlsx::writeData(wb, sheet = "metadata", x = meta)
  openxlsx::saveWorkbook(wb, file = output, overwrite = TRUE, return = TRUE)

}

                      




