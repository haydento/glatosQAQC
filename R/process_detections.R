##' utility function that extracts and summarizes receiver download data needed to produce download report##'
##' 
##' @param input data.table produced by \code{extract_records}
##'
##' @return processed data frame
##' @author Todd Hayden
##'
##
## 
## # load example vrl
## vrl <- system.file("extdata/VR2W_109412_20190619_1.vrl", package = "glatosQAQC")
##
## # set system path to vdat utility 
## if(Sys.info()["sysname"] == "Linux") vpath <- system.file("exec/vdat_linux", package = "glatosQAQC")
##
## # compile vrl files
## df <- compile_vdats(vrl, vpath)
##
## # extract detections
## df <- extract_records(df, type = "DET")
##
## # process detections necessary for download report generation.
## out <- process_detections(df)
##
##
##' @export
##' @import data.table




process_detections <- function(input){

  # stop if not correct class
#  stopifnot(all(class(input) %in% c("vdat_dtc", "data.table", "data.frame")))

    
  
  dtc <- copy(input)
  
    first_dtc <- dtc[, .SD[1],  by = .(file)]
    first_dtc[, record := "first"]
    last_dtc <- dtc[, .SD[.N], by = .(file)]
    last_dtc[, record := "last"]
    dtc <- rbind(first_dtc, last_dtc)
    dtc <- dcast(dtc, file ~ record, value.var = c("Time", "Full ID"))
  return(dtc)
}
