#' @title 
#' Convert a Vemco VRL file to a Vemco Fathom CSV file
#' 
#' @description
#' Use Vemco's VDAT command line program VDAT (distributed with Vemco Fathom
#' software) to make a CSV file containing detection data from a
#' Vemco VRL file in VDAT format.
#'
#' @param vrl_file An optional character string with path and name of a 
#'  Vemco VRL detection file, or a vector of VRL file names. If only file name 
#'  is given, then the file must be located in the working directory. 
#'  
#' @param vrl_dir An optional character string with path to a directory 
#'  containing one or more Vemco VRL detection files.    
#'  
#' @param out_dir A character string directory where CSV files will be written. 
#'   If \code{NULL} (default) then file(s) will be written to the working 
#'   directory (e.g., \code{getwd()}).
#'
#' @param overwrite Logical. If TRUE, output CSV file(s) will overwrite existing
#'   CSV file(s) with same name in \code{out_dir}. When FALSE (default),'_n'
#'   (i.e., _1, _2, etc.) will be appended to names of output files that already
#'   exist in \code{outDir}. **NOT YET IMPLEMENTED.**
#'   
#' @param recursive Logical. Indicates if VRL files in subdirectories  
#'  of \code{vrl_dir} should be converted. Default is \code{FALSE}.
#'  
#' @param vdat_path The full path to \code{VDAT} command line program.
#'   The must path includes file executable file (i.e., */.vdat.exe for Windows)
#' 
#' @param show_progress Logical. Indicates if progress bar should be shown.
#' 
#' @param diag LOgical. Indicates if diagnostic output should be included 
#'  output (default = FALSE). If TRUE, a data.frame with file size, message 
#'  and status flag (from `system2` call). 
#'
#' @details
#'  Conversion is done by system call to the Vemco program VDAT.exe. You must
#'  have VDAT.exe available at the location specified by \code{vdat_path}.
#'  VDAT.exe is included with Vemco's Fathom software.
#' 
#' @section Outputs: 
#'  A text file in Vemco Fathom CSV format for each input VRL file. 
#'  Each csv is named the same (except for extension) as the source VRL file.
#'  
#' @return A character string or vector with the name(s) (including path) of 
#'  the CSV files created.
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #get path to example Vemco VRL file
#' vrl_file <- system.file("extdata", 
#'   "VR2W_109924_20110718_1.vrl", package="glatosDS")
#' vrl_to_csv(vrl_file, out_dir = dirname(vrl_file))
#'
#' @export

#' @examples
#' vrl_file = c("C:/Users/thayden/Documents/VR2AR_546310_20190607_1.vrl", "C:/Users/thayden/Documents/VR2AR_546908_20190610_1.vrl")
#' vdat_path = c("C:/Program Files/Innovasea/Fathom/vdat.exe")
#' out_dir = "C:/Users/thayden/Desktop"
#' vrl_to_csv(vrl_file, out_dir = out_dir, vdat_path = vdat_path)

vrl_to_csv <- function(vrl_file = NULL, out_dir = NULL, 
                       vdat_path = "C:/Program Files (x86)/VEMCO/Fathom/vdat.exe"){
  # determine system OS
  os <- Sys.info()['sysname']
  
  # check to make sure vdat is installed and accessible
  vdat_ok <- suppressWarnings(system2(path.expand(vdat_path), args = "--version", stdout = FALSE, stderr = FALSE))
  if(vdat_ok == 127) stop("vdat not found.  Check path to vdat and make sure vdat software is installed and accessible to system")

  #set out_dir to source file location if not given
  if(is.null(out_dir)) out_dir <- ifelse(is.null(vrl_dir), 
                                         dirname(vrl_file), vrl_dir)

  out_dir <- normalizePath(out_dir)
  
  #output path(s) and file name(s)
  out_file <- file.path(out_dir, 
                        gsub(".vrl|.VRL|.Vrl",".csv", basename(vrl_file)))

  #loop through files so that progress can be displayed
  message(paste0("Converting ", length(vrl_file), " VRL file(s) to VDAT CSV..."))

  # prepare vdat command
  cmd <- path.expand(file.path(vdat_path))

  # prepare arguments to command line call
  args <- c("convert", "--format=csv.fathom", "--timec=default", paste("--output", shQuote(out_dir)), shQuote(vrl_file))

  msg <- system2(cmd, args)
  
  # message == 0 is no errors.
  out <- data.frame(stringsAsFactors = FALSE,
                    output_file = normalizePath(out_file), 
                    file_size = file.size(out_file),
                    status = msg)

  return(out)

  message("\nConversion complete.")

}
