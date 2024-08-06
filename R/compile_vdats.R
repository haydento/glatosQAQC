# the "compile_vdats" function uses vdat utility to open each vrl, converts to csv, and combines detections, and extracts the "DET", "EVENT_OFFLOAD", and "EVENT_INIT" data for use in report.  Input is a vector of file paths to the VRL files that are of interest, a path to vdat, and a temporary directory to save intermediate .csv files converted from the VRL.

# this function decodes one or more vrl files and creates a list for each vrl
# The list contains a separate data frame for each data type in the vrl
##' Converts vrl to csv using Innovasea vdat command line utility.
##'
##' @param vdat_files a character vector of one or more file paths pointing to the vrl files that will be included in the download report
##' @param v_path a character file path pointing to the vdat utility.
##' @param temp_dir A folder where converted vdat files will be saved.  Defaults to system temporary directory.
##' @return A list object that contains one element for each vrl submitted in \code{vdat_files}  
##' @author Todd Hayden

##' @examples

##' # load example vrl
##' vrl <- system.file("extdata/VR2W_109412_20190619_1.vrl", package = "glatosQAQC")
##'
##' # set system path to vdat utility 
##' if(Sys.info()["sysname"] == "Linux") vpath <- system.file("exec/vdat_linux", package = "glatosQAQC")
##'
##' # compile vrl files
##'
##' tst <- vrl_to_csv(vrl_file = vrl_file, out_dir = tempdir(), vdat_path = vpath)
##'
##' 
##' foo <- compile_vdats(vdat_files = vrl, v_path = vpath)




#' vdat_files = c(
#' "C:/Users/thayden/Desktop/QAQC_weirdness/VR2AR_547562_20220906_1.vrl",
#' "C:/Users/thayden/Desktop/QAQC_weirdness/VR2Tx_480029_20220906_1.vrl",
#' "C:/Users/thayden/Desktop/QAQC_weirdness/VR2Tx_483620_20220906_1.vrl",
#' "C:/Users/thayden/Desktop/QAQC_weirdness/VR2Tx_483626_20220906_1.vrl",
#' "C:/Users/thayden/Desktop/QAQC_weirdness/VR2Tx_483638_20220906_1.vrl",
#' "C:/Users/thayden/Desktop/QAQC_weirdness/VR2Tx_483813_20220906_1.vrl" ,
#' "C:/Users/thayden/Documents/VR2AR_546310_20190607_1.vrl")
#' vdat_files <- system.file("extdata/VR2W_109412_20190619_1.vrl", package = "glatosQAQC")
#'
#' vdat_files <- c(
#' "C:/Users/thayden/Documents/VR2AR_Samples_with_Gen2/new/VR2AR_546906_20230516_1.vrl",
#' "C:/Users/thayden/Documents/VR2AR_Samples_with_Gen2/new/VR2AR_547544_20230516_1.vrl"
#' )
#'
#' vdat_files <- c(
#' "C:/Users/thayden/Documents/VR2AR_Samples_with_Gen2/new/VR2AR_546906_20230516_1.vrl"
#' )

#' 
#' v_path = c("C:/Program Files/Innovasea/Fathom/vdat.exe")
#' temp_dir = tempdir()
#' foo <- compile_vdats(vdat_files = vdat_files, v_path = v_path, temp_dir = temp_dir)
#'
#'
#' vdat_files <- c("C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20190607_1.vrl", "C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20220624_1.vrl", "C:/Users/Admin/Documents/VRL_tests/VR2W_134214_20230626_1.vrl")
 




##' @export

#' vdat_files <- c("C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20190607_1.vrl", "C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20220624_1.vrl", "C:/Users/Admin/Documents/VRL_tests/VR2W_134214_20230626_1.vrl")
#' library(rvdat)
#' library(data.table)
#' vdat_here("C:/Program Files/Innovasea/Fathom Connect/vdat.exe")
#'
#'# must check for unity among tables from receivers.  Suspect I will need to use a schema with column types to do this.
#' tst <- compile_vdats(vdat_files, tbls = c("DET.csv", "BATTERY.csv", "DATA_SOURCE_FILE.csv", "CLOCK_REF.csv", "EVENT_OFFLOAD.csv", "EVENT_INIT.csv"))
#'
#' 

compile_vdats <- function(vdat_files, tbls = c("DET.csv", "BATTERY.csv")){
  t_dir <- tempdir()
  out_dirs <- file.path(t_dir, gsub("\\.vrl", ".csv-fathom-split", basename(fls)))
  lapply(vdat_files, rvdat::vdat_to_csv, outdir = t_dir, time_corrected = TRUE, quiet = TRUE, folder = TRUE, filter = NULL)
  ext_tbl <- lapply(tbls, function(x)file.path(out_dirs, x))
  out <- vector(mode = "list", length = length(ext_tbl))
  names(out) <- tbls
  
  for(i in 1:length(ext_tbl)){
    out[[i]] <- rbindlist(lapply(ext_tbl[[i]], fread), fill = TRUE, idcol = "file")
  }

  unlink(out_dirs, recursive = TRUE)
  
  return(out)
}


