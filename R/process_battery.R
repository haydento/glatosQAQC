##' utility function that extracts and summarizes receiver download data needed to produce download report
##'
##' 
##' @param input data.table produced by \code{extract_records}
##'
##' @return processed data frame
##' @author Todd Hayden
##'
##' @examples
##' 
##' # load example vrl
##' vrl <- system.file("extdata/VR2W_109412_20190619_1.vrl", package = "glatosQAQC")
##'
##' # set system path to vdat utility 
##' if(Sys.info()["sysname"] == "Linux") vpath <- system.file("exec/vdat_linux", package = "glatosQAQC")
##'
##' # compile vrl files
##' df <- compile_vdats(vrl, vpath)
##'
##' # extract detections
##' df <- extract_records(df, type = "DET")
##'
##' # process detections necessary for download report generation.
##' out <- process_detections(df)
##'
##'
##' @export
##' @import data.table
process_detections_battery <- function(input){

  stopifnot(all(class(input) %in% c("vdat_bat", "data.table", "data.frame")))
  
  dtc <- copy(input)

  # all <- CJ(file = bat$file, `Battery Position` = c("PRIMARY", "MOTOR"), record = c("first", "last"), unique = TRUE)
  dtc <- dtc[, .SD[c(1,.N),],  by = .(file, `Battery Position`)]
  first_bat <- dtc[, .SD[1,], by = .(file, `Battery Position`)]
  first_bat[, record := "first"]
  last_bat <- dtc[, .SD[.N], by = .(file, `Battery Position`)]
  last_bat[, record := "last"]
  dtc <- rbind(first_bat, last_bat)
  dtc <- dcast(dtc, file + `Battery Position` ~  record, value.var = c("Time", "Battery Voltage (V)"))
  
return(dtc)
}
