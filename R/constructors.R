# class constructors for QAQC, validators, and helper functions
# each table used in QAQC script has its own class
#' @export

# validator for detections table
# x = col names
# n = rows
## vdat_dtc <- function(x, n, row.names = NULL) {
##   # check if underlying object is a list
##   stopifnot(is.list(x))

##   # check all inputs are the same length
##   stopifnot(all(lengths(x) == n))

##   if(is.null(row.names)){
##     row.names <- .set_row_names(x)
##   } else {
##     stopifnot(is.character(row.names), length(row.names) == n)
##   }

##   structure(
##     x,
##     class  = c("vdat_dtc", "data.table", "data.frame"),
##     row.names = row.names
##   )
## }

#' library(rvdat)
#' vdat_template(format = "csv.fathom")


vdat_dtc <- function(x, template){
  stopifnot(is.data.table(x), is.data.frame(x))
  colnames = 

  #colnames <- c("file", "Device Time (UTC)", "Time", "Time Offset (h)", "Time Correction (s)", "Mo#del", "Serial Number", "Channel", "Detection Type", "Full ID", "ID", "Raw Data", "Transmitter Seri#al", "Signal Strength (dB)", "Noise (dB)", "Gain (dB)", "Quality Score", "Station Name", "Latitude#", "Longitude", "GPS HDOP")

  missing_cols <- unique(setdiff(names(x), colnames))
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing, "'", collapse = ","), ".", call. = FALSE)
  }

  # set character columns
  set_char <- c("file", "Model", "Detection Type", "Full ID", "Quality Score", "Station Name")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))

  # set POSIXct (UTC tz)
  set_pos_utc <- c("Device Time (UTC)", "Time")
  for(col in set_pos_utc)
    set(x, j = col, value = as.POSIXct(x[[col]], tz = "UTC"))

  # set numeric columns
  set_num <- c("Time Offset (h)", "Time Correction (s)", "Signal Strength (dB)", "Noise (dB)", "Gain (dB)", "Latitude", "Longitude", "GPS HDOP")
  for(col in set_num)
    set(x, j = col, value = as.numeric(x[[col]]))

  # set integer columns
  set_int <- c("Serial Number", "Channel", "Raw Data", "Transmitter Serial")
  for(col in set_int)
    set(x, j = col, value = as.integer(x[[col]]))

  
#Assign class
  x <- structure(x, 
                 class = c("vdat_dtc", class(x)))

  return(x)
}


#################
#x <- bat

#x <- x[0,]

#' @export
vdat_bat <- function(x){
  stopifnot(is.data.table(x), is.data.frame(x))
  colnames <- c("file", "Device Time (UTC)", "Time", "Time Offset (h)", "Time Correction (s)", "Model", "Serial Number", "Battery Position", "Battery Type", "Battery Serial Number", "Battery Voltage (V)", "Battery Remaining (%)")
                
  missing_cols <- unique(setdiff(names(x), colnames))
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing, "'", collapse = ","), ".", call. = FALSE)
  }

  # set character columns
  set_char <- c("file", "Model", "Battery Position", "Battery Type", "Battery Serial Number")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))

  # set POSIXct (UTC tz)
  set_pos_utc <- c("Device Time (UTC)", "Time")
  for(col in set_pos_utc)
    set(x, j = col, value = as.POSIXct(x[[col]], tz = "UTC"))

  # set numeric columns
  set_num <- c("Time Offset (h)", "Time Correction (s)", "Battery Voltage (V)", "Battery Remaining (%)")
  for(col in set_num)
    set(x, j = col, value = as.numeric(x[[col]]))

  # set integer columns
  set_int <- c("Serial Number")
  for(col in set_int)
    set(x, j = col, value = as.integer(x[[col]]))
ark <- x
 
  # handle an empty data.table
  if(nrow(x) == 0){
    x <- data.table(file = NA_character_, Time = as.POSIXct(NA, tz = "UTC"), MOTOR = NA_real_, PRIMARY = NA_real_)
  } else {

    x <- data.table::dcast(x, file + Time ~ `Battery Position`, value.var = "Battery Voltage (V)")
    req_cols <- c("file", "Time", "PRIMARY", "MOTOR")
    col_difs <- setdiff(req_cols, names(x))
    if(length(col_difs) > 0){
      x[, (col_difs) := NA_real_]
    }
  }

  #Assign class
  x <- structure(x, 
                 class = c("vdat_bat", class(x)))

  return(x)
}



#' constructor function for integrated tag
#' @examples
#' i_tag <- glatosQAQC::extract_records(vdat = dtc, type = "CFG_TRANSMITTER")
#' vdat_i_tag(i_tag)
#' @export

vdat_i_tag <- function(x){
  stopifnot(is.data.table(x), is.data.frame(x))

  if(nrow(x) == 0){
    x <- data.table(file = NA_character_,
                    `Device Time (UTC)` = as.POSIXct(NA, tz = "UTC"),
                    Time = as.POSIXct(NA, tz = "UTC"),
                    `Time Offset` = NA_integer_,
                    `Time Correction (s)` = NA_integer_,
                    Model = NA_character_,
                    `Serial number` = NA_character_,
                    `Transmission Type` = NA_character_,
                    `Full ID` = NA_character_,
                    ID = NA_integer_,
                    `Power Level` = NA_character_,
                    `Min Delay (s)` = NA_integer_,
                    `Max Delay (s)` = NA_integer_, )
  

  return(x)
}

  colnames <- c("file", "Device Time (UTC)", "Time", "Time Offset (h)", "Time Correction (s)", "Model", "Serial Number", "Transmission Type", "Full ID", "ID", "Power Level", "Min Delay (s)", "Max Delay (s)")

  missing_cols <- unique(setdiff(names(x), colnames))
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing, "'", collapse = ","), ".", call. = FALSE)
  }

  # set character columns
  set_char <- c("file", "Model", "Transmission Type", "Full ID", "Power Level")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))

  # set POSIXct (UTC tz)
  set_pos_utc <- c("Device Time (UTC)", "Time")
  for(col in set_pos_utc)
    set(x, j = col, value = as.POSIXct(x[[col]], tz = "UTC"))

  # set numeric columns
  set_num <- c("Time Offset (h)", "Time Correction (s)")
  for(col in set_num)
    set(x, j = col, value = as.numeric(x[[col]]))

  # set integer columns
  set_int <- c("Serial Number", "ID", "Min Delay (s)", "Max Delay (s)")
  for(col in set_int)
    set(x, j = col, value = as.integer(x[[col]]))

  # extract receiver model and serial number from file name
x[, Model := tstrsplit(file, "_", keep = 1)]
x[, `Serial Number` := tstrsplit(file, "_", keep = 1)]
  
#Assign class
  x <- structure(x, 
                 class = c("vdat_itag", class(x)))
  return(x)
}



