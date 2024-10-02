#' constructor function for data_source_file
#' @export

#x <- file_id[5]

as.data_source_file <- function(x){
  if(is.null(x[[1]])){
    x <- data.table(`File Name` = NA_character_,
                    `Size` = NA_integer_,
                    `State` = NA_character_)
    return(x)
  }

  stopifnot(is.data.table(x), is.data.frame(x))

  # required column names
  req_cols <- c("File Name", "Size", "State")
  #missing_cols <- unique(setdiff(names(x), colnames))

  missing_cols <- unique(setdiff(req_cols, names(x)))

  # x <- c("File Name", "Size", "State", "x")
  # x <- c("Size", "State", "x")
  # unique(setdiff(colnames,x))
  
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing_cols, "'", collapse = ","), ".", call. = FALSE)
  }

  # extract only required columns
  x <- x[, ..req_cols]
  
  # set character columns
  set_char <- c("File Name", "State")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))

  # set integer columns
  set_int <- c("Size")
  for(col in set_int)
    set(x, j = col, value = as.integer(x[[col]]))

#Assign class
  x <- structure(x, 
                 class = c("data_source_file", class(x)))
  return(x)
}


###############
#' constructor function for data_source_file
#' @export

as.det <- function(x){
  if(is.null(x[[1]])){
    x <- data.table(`Time` = as.POSIXct(NA, tz = "UTC"),
                    `Full ID` = NA_character_)
    return(x)
  }

  stopifnot(is.data.table(x), is.data.frame(x))

  # required column names
  req_cols <- c("Time", "Full ID")
  #missing_cols <- unique(setdiff(names(x), colnames))

  missing_cols <- unique(setdiff(req_cols, names(x)))

  # x <- c("File Name", "Size", "State", "x")
  # x <- c("Size", "State", "x")
  # unique(setdiff(colnames,x))
  
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing_cols, "'", collapse = ","), ".", call. = FALSE)
  }

  # extract only required columns
  x <- x[, ..req_cols]
  
  # set character columns
  set_char <- c("Full ID")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))

  # set integer columns
  set_posix <- c("Time")
  for(col in set_posix)
    set(x, j = col, value = as.POSIXct(x[[col]], tz = "UTC"))

#Assign class
  x <- structure(x, 
                 class = c("det", class(x)))
  return(x)
}

####################
#' constructor function for cfg_channel
#' @export
as.cfg_channel <- function(x){
  if(is.null(x[[1]])){
    x <- data.table(`Map ID` = NA_character_,
                    `Model` = NA_character_,
                    `Serial Number` = NA_integer_,
                    `Frequency (kHz)` = NA_integer_)
    return(x)
  }

  stopifnot(is.data.table(x), is.data.frame(x))

  # required column names
  req_cols <- c("Map ID", "Model", "Serial Number", "Frequency (kHz)")
  #missing_cols <- unique(setdiff(names(x), colnames))

  missing_cols <- unique(setdiff(req_cols, names(x)))

  # x <- c("File Name", "Size", "State", "x")
  # x <- c("Size", "State", "x")
  # unique(setdiff(colnames,x))
  
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing_cols, "'", collapse = ","), ".", call. = FALSE)
  }

  # extract only required columns
  x <- x[, ..req_cols]
  
  # set character columns
  set_char <- c("Map ID", "Model")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))

  # set integer columns
  set_int <- c("Serial Number", "Frequency (kHz)")
  for(col in set_int)
    set(x, j = col, value = as.integer(x[[col]]))

#Assign class
  x <- structure(x, 
                 class = c("cfg_channel", class(x)))
  return(x)
}

#' constructor function for clock_ref
#' @export

as.clock_ref <- function(x){
  template <- data.table(`Time` = as.POSIXct(NA, tz = "UTC"),
                         `External Time (UTC)` = as.POSIXct(NA, tz = "UTC"),
                         `Source` = c("INITIALIZATION", "OFFLOAD"))
  
  if(is.null(x[[1]])){   
    return(template)
  }

  stopifnot(is.data.table(x), is.data.frame(x))

  # required column names
  req_cols <- c("Time", "External Time (UTC)", "Source")
  #missing_cols <- unique(setdiff(names(x), colnames))

  # extract only required columns
  x <- x[, ..req_cols]
 
  # determine if any required columns are missing
  missing_cols <- unique(setdiff(req_cols, names(x)))

  # x <- c("File Name", "Size", "State", "x")
  # x <- c("Size", "State", "x")
  # unique(setdiff(colnames,x))
  
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing_cols, "'", collapse = ","), ".", call. = FALSE)
  }

  # should always have one initialization and one offload record- add one if not present, set to NA
  req_rows <- c("INITIALIZATION", "OFFLOAD")
  missing_rows <- unique(setdiff(req_rows, x$Source))
  if(length(missing_rows) > 0) {
    x <- rbind(x, template[Source == missing_rows,])
    setkey(x, Source)
  }
    
  # set character columns
  set_char <- c("Source")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))

  # set POSIXct columns
  set_posix <- c("Time", "External Time (UTC)")
  for(col in set_posix)
    set(x, j = col, value = as.POSIXct(x[[col]], tz = "UTC"))

#Assign class
  x <- structure(x, 
                 class = c("clock_ref", class(x)))
  return(x)
}


###########
#' constructor function for event_init
#' @export

as.event_init <- function(x){
  if(is.null(x[[1]])){
    x <- data.table(`Firmware Version` = NA_character_)
    return(x)
  }

  stopifnot(is.data.table(x), is.data.frame(x))

  # required column names
  req_cols <- c("Firmware Version")
  #missing_cols <- unique(setdiff(names(x), colnames))
  missing_cols <- unique(setdiff(req_cols, names(x)))

  # x <- c("File Name", "Size", "State", "x")
  # x <- c("Size", "State", "x")
  # unique(setdiff(colnames,x))
  
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing_cols, "'", collapse = ","), ".", call. = FALSE)
  }

  # extract only required columns
  x <- x[, ..req_cols]
  
  # set character columns
  set_char <- c("Firmware Version")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))

#Assign class
  x <- structure(x, 
                 class = c("event_init", class(x)))
  return(x)
}

###############
#' constructor function for event_offload
#' @export

as.event_offload <- function(x){

  if(is.null(x[[1]])){
    x <- data.table(`PPM Total Accepted Detections` = NA_integer_,
                    `Memory Remaining (%)` = NA_integer_,
                    `Battery Remaining (%)` = NA_integer_,
                    `Original File` = NA_character_)
    return(x)
  }

  stopifnot(is.data.table(x), is.data.frame(x))

  # required column names
  req_cols <- c("PPM Total Accepted Detections", "Memory Remaining (%)", "Battery Remaining (%)", "Original File")
  #missing_cols <- unique(setdiff(names(x), colnames))
  missing_cols <- unique(setdiff(req_cols, names(x)))

  # x <- c("File Name", "Size", "State", "x")
  # x <- c("Size", "State", "x")
  # unique(setdiff(colnames,x))
  
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing_cols, "'", collapse = ","), ".", call. = FALSE)
  }

  # extract only required columns
  x <- x[, ..req_cols]
  
  # set integer columns
  set_int <- c("Memory Remaining (%)", "Battery Remaining (%)", "PPM Total Accepted Detections")
  for(col in set_int)
    set(x, j = col, value = as.integer(x[[col]]))

  # set character columns
  set_char <- c("Original File")
  for(col in set_char)
    set(x, j = col, value = as.character(x[[col]]))
  
#Assign class
  x <- structure(x, 
                 class = c("event_offload", class(x)))
  return(x)
}


#' integrated tag constructor function
as.cfg_transmitter <- function(x){

  if(is.null(x[[1]]) ){
    x <- data.table(CFG_TRANSMITTER_DESC = NA_character_,
                    `Device Time (UTC)` = as.POSIXct(NA, tz = "UTC"),
                    Time = as.POSIXct(NA, tz = "UTC"),
                    `Time Offset (h)` = NA_real_,
                    `Time Correction (s)` = NA_real_,
                    Model = NA_character_,
                    `Serial Number` = NA_character_,
                    `Transmission Type` = NA_character_,
                    `Full ID` = NA_character_,
                    ID = NA_integer_,
                    `Power Level` = NA_character_,
                    `Min Delay (s)` = NA_integer_,
                    `Max Delay (s)` = NA_integer_ )
  

  return(x)
}

  stopifnot(is.data.table(x), is.data.frame(x))

  
  colnames <- c("CFG_TRANSMITTER_DESC", "Device Time (UTC)", "Time", "Time Offset (h)", "Time Correction (s)", "Model", "Serial Number", "Transmission Type", "Full ID", "ID", "Power Level", "Min Delay (s)", "Max Delay (s)")

  missing_cols <- unique(setdiff(names(x), colnames))
  if (length(missing_cols) > 0) {
    stop(
      "the following columns are not found: ",
      paste0("'", missing, "'", collapse = ","), ".", call. = FALSE)
  }

  # set character columns
  set_char <- c("Model", "Transmission Type", "Full ID", "Power Level", "Serial Number")
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
  set_int <- c("ID", "Min Delay (s)", "Max Delay (s)")
  for(col in set_int)
    set(x, j = col, value = as.integer(x[[col]]))

  # extract receiver model and serial number from file name
#x[, Model := tstrsplit(file, "_", keep = 1)]
#x[, `Serial Number` := tstrsplit(file, "_", keep = 1)]
  
#Assign class
  x <- structure(x, 
                 class = c("cfg_transmitter", class(x)))
  return(x)
}



