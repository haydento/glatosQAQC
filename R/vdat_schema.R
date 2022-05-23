#' @title Schema that defines column data type output for "read_vdat_csv" function
#' @description  This function looks up appropriate data types for ONLY columns needd and used in QAQC app.  Many columns output in vdat are not included(!).  Data.table::fread will guess the appropriate data types for columns not included in this table.
#' @param in_col_name vector of column names
#' @return vector of data types that can be passed on to 'colClasses' argument in data.table::fread

#col_class_out(c("Device Time (UTC)", "time", "Time", "Noise (dB)"))
## schema_def$sch_col_type[match(names(header$DET), schema_def$sch_col_name)]

#' @export

col_class_out <- function(in_col_name){

  schema_def <- data.table(sch_col_name = c("Device Time (UTC)",
                                            "Time",
                                            "Time Offset (h)",
                                            "Time Correction (s)",
                                            "Model",
                                            "Serial Number",
                                            "Transmission Type",
                                            "Full ID",
                                            "ID",
                                            "Power Level",
                                            "Min Delay (s)",
                                            "Max Delay (s)",
                                            "Channel",
                                            "Detection Type",
                                            "Raw Data",
                                            "Transmitter Serial",
                                            "Signal Strength (dB)",
                                            "Noise (dB)",
                                            "Gain (dB)",
                                            "Quality Score",
                                            "Station Name",
                                            "Latitude",
                                            "Longitude",
                                            "GPS HDOP",
                                            "File Name",
                                            "UUID",
                                            "Type",
                                            "Size",
                                            "State",
                                            "Battery Position",
                                            "Battery Type",
                                            "Battery Serial Number",
                                            "Battery Voltage (V)",
                                            "Battery Remaining (%)",
                                            "Frequency (kHz)",
                                            "Blanking (ms)",
                                            "Map ID",
                                            "Coding ID",
                                            "External Time (UTC)",
                                            "External Difference (s)",
                                            "Source",
                                            "External Time Zone",
                                            "HR Total Accepted Detections",
                                            "PPM Total Accepted Detections",
                                            "Memory Remaining (%)",
                                            "Original File"
                                            ),
                           sch_col_type = c("POSIXct",
                                            "POSIXct",
                                            "numeric",
                                            "numeric",
                                            "character",
                                            "integer",
                                            "character",
                                            "character",
                                            "integer",
                                            "character",
                                            "integer",
                                            "integer",
                                            "integer",
                                            "character",
                                            "integer",
                                            "integer",
                                            "numeric",
                                            "numeric",
                                            "numeric",
                                            "character",
                                            "character",
                                            "numeric",
                                            "numeric",
                                            "numeric",
                                            "character",
                                            "character",
                                            "character",
                                            "integer",
                                            "character",
                                            "character",
                                            "character",
                                            "character",
                                            "numeric",
                                            "numeric",
                                            "integer",
                                            "integer",
                                            "character",
                                            "character",
                                            "POSIXct",
                                            "numeric",
                                            "character",
                                            "character",
                                            "integer",
                                            "integer",
                                            "numeric",
                                            "character"
                                            )
                           )

  out <- schema_def$sch_col_type[match(in_col_name, schema_def$sch_col_name)]
return(out)
}


                                          
                                          
                                          
