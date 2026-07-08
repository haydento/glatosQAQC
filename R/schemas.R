# schema defs and S7 class defs

# schema that maps output names to inputs obtained from vdat call.
# only include column names spec'd in output table and columns used to create output table (i.e., derived columns)
  out_names <- function(){
    list(
      DET = list(`first det` = "Time_min",
                 `last det` = "Time_max"),    
      CFG_CHANNEL = list(`rec map` = "Map ID",
                         `rec mod` = "Model", 
                         `rec num` = "Serial Number",
                         `rec freq` = "Frequency (kHz)"),
      CLOCK_REF = list(`rec init` = "Time_INITIALIZATION",
                       `rec download` = "Time_OFFLOAD",
                       `comp download` = "External Time (UTC)_OFFLOAD"),
      EVENT_INIT = list(`rec firmware` = "Firmware Version"),
      EVENT_OFFLOAD = list(`num det` = "PPM Total Accepted Detections",
                           `mem avail` = "Memory Remaining (%)",
                           `battery (%)` = "Battery Remaining (%)",
                           `name` = "Original File"),
      CFG_TRANSMITTER = list(`int tag ID` = "Full ID",
                             `int tag power` = "Power Level",
                             `int tag delay min` = "Min Delay (s)",
                             `int tag delay max` = "Max Delay (s)")
    )
  }

# Columns in the preferred order included in output table.
table_cols <- function(){
  cols = c(
    "name",
    "rec num",
    "rec mod",
    "rec firmware",
    "rec map",
    "mem avail",
    "battery (%)",
    "rec init",
    "rec download",
    "comp download",
    "first det",
    "last det",
    "num det"
  )
}

# create schema object
# vdat S7 class schema function
# this schema defines column types when data is brought in
vdat_schema <- function(){
  list(
    DET = list("Full ID" = "character",
               "Time" = "POSIXct"),    
    CFG_CHANNEL = list("Map ID" = "character",
                       "Model" = "character",
                       "Serial Number" = "integer",
                       "Frequency (kHz)" = "integer"),
    CLOCK_REF = list("Time" = "POSIXct",
                     "External Time (UTC)" = "POSIXct",
                     "Source" = "character"),
    EVENT_INIT = list("Firmware Version" = "character"),
    EVENT_OFFLOAD = list("PPM Total Accepted Detections" = "integer",
                         "Memory Remaining (%)" = "integer",
                         "Battery Remaining (%)" = "character",
                         "Original File" = "character"),
    CFG_TRANSMITTER = list("Device Time (UTC)" = "POSIXct",
                           "Time" = "POSIXct",
                           "Time Offset (h)" = "numeric",
                           "Time Correction (s)" = "numeric",
                           "Model" = "character",
                           "Serial Number" = "character",
                           "Transmission Type" = "character",
                           "Full ID" = "character",
                           "ID" = "integer",
                           "Power Level" = "character",
                           "Min Delay (s)" = "integer",
                           "Max Delay (s)" = "integer"),
    DATA_SOURCE_FILE = list("File Name" = "character",
                            "Size" = "integer",
                            "State" = "character"))
}
