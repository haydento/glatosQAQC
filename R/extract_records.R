##' Extract records contained in VRL files for report

##' @param vdat List of vdat files from csv created by \code{compile_vdats}
##' @param type name of data types to be extracted.
##' @return a data.table containing all of records extracted from csv file
##' @author Todd Hayden
##'
##' @examples
##'
##'
##' # load example vrl
##' vrl <- system.file("extdata/VR2W_109412_20190619_1.vrl", package = "glatosQAQC")
##'
##' # set system path to vdat utility 
##' if(Sys.info()["sysname"] == "Linux") vpath <- system.file("vdat/vdat_linux", package = "glatosQAQC")
##'
##' # compile vrl files
##' df <- compile_vdats(vrl, vpath)
##'
##' # extract detections
##' df <- extract_records(df, type = "DET")
##'
##' 
##' @export
##' @import data.table


extract_records <- function(vdat, type = "DET"){

##   # see stackoverflow question 6941506
##   Filter(function(x) nrow > 0, vdat
## lapply(ark, length)  

  
  # create combined data.table object for detections from all files
  dtc <- data.table::rbindlist(lapply(vdat, "[[", type), idcol = "file")

  # creates NAs for all columns except file name and appends to table.
  # This is only neede if no detections are recorded (should be rare). 
  missing <- setdiff(names(vdat), unique(dtc$file))
  
  if(length(missing) > 0){
    dtc <-  rbind(data.table(file = missing), dtc, fill = TRUE)
  }
  
  # change file ending to .vrl instead of .csv
  dtc[, file := gsub(".csv", ".vrl", file, fixed = TRUE)]
  
  return(dtc)
}
