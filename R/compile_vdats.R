#' the "compile_vdats" function uses vdat utility to open each vrl, convert each file to a folder of .csv files containing all the data stored on the receiver.  By default, data are extracted and written to a unique directory in a temporary location.  Innovasea Vdat command line program must be installed on computer and on path, Function uses a modified version of the rvdat package.

##' Converts vrl to csv using Innovasea vdat command line utility.
##'
##' @param vdat_file a character vector of one or more file paths pointing to the vrl files that will be extracted
##' @param vdat_pth file path to vdat.exe executable
##' @return A list object that contains file path to directory containing all csv files extracted from vrl in \code{vdat_files}  
##' @author Todd Hayden


#' fls <- c("C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20190607_1.vrl",
#'         "C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20190607_1.vrl",
#'         "C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20220624_1.vrl",
#'         "C:/Users/Admin/Documents/VRL_tests/VR2W_134214_20230626_1.vrl")

#'

#' tst <- .vdat_compile(vdata_file = fls[1])
#'
#' Multiple files extracted simultaneously:
#' tst <- lapply(fls, .vdat_compile)

## .vdat_compile <- function(vdata_file,
##                           vdat_pth = NULL) {
##   if(is.null(vdat_pth)){
##     vdat_pth = glatosQAQC::check_vdat()
##   }

##     #out_dir <- tempfile()
##     #dir.create(out_dir)
    
##     ## Convert function arguments
##     vdat_args <- c(
##       "convert",
##       "--format=csv.fathom",
##       vdata_file,
##      # paste0("--output=", out_dir),
##       "--timec=default"
##     )
    
##     ## filename <- gsub(
##     ##   "vdat|vrl",
##     ##   #"csv-fathom-split",
##     ##   "csv.fathom"
##     ##   basename(vdata_file)
##     ## )

##     #time_shell_out <- format(Sys.time(), "[%s]")
##     shell_out <- sys::exec_internal(cmd = vdat_pth, args = vdat_args, error = FALSE)
    
##     # Handle error
##     #if (shell_out$status == 1) {
##      # stop(shell_out$stderr)    
##     #}
##   #out <- file.path(out_dir, filename)
##   vdata_file
##     return(vdata_file)
## }


#' @export

.compile <- function(vdata_file, vdat_pth){
    
    ## Convert function arguments
    vdat_args <- c(
      "convert",
      "--format=csv.fathom",
      vdata_file,
     # paste0("--output=", out_dir),
      "--timec=default"
    )
    
  #time_shell_out <- format(Sys.time(), "[%s]")
  shell_out <- sys::exec_internal(cmd = vdat_pth, args = vdat_args, error = FALSE)  
  return(vdata_file)
}
