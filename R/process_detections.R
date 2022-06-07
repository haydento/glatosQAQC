##' utility function that extracts and summarizes receiver download data needed to produce download report
##'
##' 
##' @param input data.table produced by \code{extract_records}
##' @param battery boolean, set battery = TRUE when processing records for battery voltage
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


## process_detections <- function(input){

##   # copy inputs for data.table
##   dtc <- copy(input)

##   # extract first and last rows of detections
##   # and format
    
##     # extract first and last detections for each receiver
##     data.table::setkey(dtc, file, `Device Time (UTC)`)
##     dtc <- dtc[, .SD[c(1,.N),], by = .(file)]

##     # assign first and last designators
##     setkey(dtc, file, `Device Time (UTC)`)
##     dtc[, dtc_order := c("first_detect", "last_detect"), by = .(file)]

##   # create wide table of first and last detections
##   dtc <- dcast(dtc, file ~ dtc_order, value.var = c("Device Time (UTC)", "Full ID"))

##   return(dtc)
## }



process_detections <- function(input){

  stopifnot(all(class(input) %in% c("vdat_dtc", "data.table", "data.frame")))
  
  dtc <- copy(input)
  
    first_dtc <- dtc[, .SD[1],  by = .(file)]
    first_dtc[, record := "first"]
    last_dtc <- dtc[, .SD[.N], by = .(file)]
    last_dtc[, record := "last"]
    dtc <- rbind(first_dtc, last_dtc)
    dtc <- dcast(dtc, file ~ record, value.var = c("Time", "Full ID"))
  return(dtc)
}
