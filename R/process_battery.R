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

process_detections_battery <- function(input){

  stopifnot(all(class(input) %in% c("vdat_bat", "data.table", "data.frame")))
  
  dtc <- copy(input)
  dtc <- melt(dtc, id.vars = c("file", "Time"), measure.vars = c("MOTOR", "PRIMARY"))
  dtc <- dtc[dtc[, .(idx = .I[c(.N)]), by = .(file, variable)]$idx,]
  dtc <- dcast(dtc, file  ~  variable, value.var = c("value"))
  return(dtc)
}
