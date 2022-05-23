#' @title Read detections and events from an Innovasea Fathom VDAT CSV file
#'
#' @description Read detections and events from a Innovasea Fathom VDAT CSV file
#'
#' @param src A character string with path and name of a Innovasea VDAT CSV
#'   detection file. If only file name is given, then the file must be located
#'   in the working directory.
#'
#' @param record_types An optional vector of character strings with names of
#'   record types to read from the file. E.g., "DET" for detection records.
#'   Default (\code{NULL}) will read all record types present in input CSV
#'   \code{src}.
#'
#' @details Reading is done via \code{\link[data.table]{fread}}.
#'
#' @details All timestamp columns are assumed to be in UTC.
#'
#' @return A list of class \code{vdat_list} with one named element for each
#'   record type and attributes: \code{fathom_csv_format_version} with version
#'   of the input Fathom CSV format; \code{vat_exe_version} = with version of
#'   VDAT.exe used to create input CSV.
#'
#'
#' @author C. Holbrook (cholbrook@@usgs.gov)
#'
#' @examples
#' vrl_file <- system.file("extdata",
#'   "VR2W_109924_20110718_1.vrl", package="glatos")
#'
#' csv_file <- vrl_to_csv(vrl_file, out_dir = dirname(vrl_file))
#'
#' #read all record types
#' vdat <- read_vdat_csv(csv_file)
#' 
#' #read only two record types
#' vdat <- read_vdat_csv(csv_file, record_types = c("DATA_SOURCE_FILE", "DET"))
#'
#' @export


## vrl_file <- c("C:/Users/thayden/Documents/VR2W_109412_20190619_1.vrl")
## vdat_path = "C:/Program Files/Innovasea/Fathom/vdat.exe"
## csv_file <- vrl_to_csv(vrl_file, out_dir = dirname(vrl_file), vdat_path = vdat_path )
## src = csv_file$output
## record_types = "DET"
## foo <- read_vdat_csv(src = csv_file$output, record_types = NULL)

read_vdat_csv <- function(src, record_types = NULL){
  
  #Identify vdat csv format version and vdat.exe version that created input csv
  src_version <- data.table::fread(src, nrows = 1L, header = FALSE, 
                                   drop = 1, 
                                   col.names = c("fathom_csv", "vdat_exe"))
  
  #Read all data into character vector (like readLines)
  vdat_txt <- data.table::fread(file = src, skip = 2, header = FALSE, 
                                sep = NULL, col.names = "txt")

  #Identify record type of each row
  vdat_txt[ , record_type := data.table::tstrsplit(txt, 
                                                   split = ",", 
                                                   keep = 1)]
  
  vdat_txt[ , record_type := gsub("_DESC$", "", record_type)]  
  
  
  #Get record identifiers from csv file
  csv_record_types <- unique(vdat_txt$record_type)


  if(is.null(record_types)) {
    
    record_types <- csv_record_types 

  } else {
    
    #Check if any record_types are not in csv
    unknown_record_types <- setdiff(record_types, csv_record_types)
    
    if(length(unknown_record_types) > 0) stop("The following input ",
                                       "'record_types' ",
                                       "were not found in CSV file: \n\t",
                                       paste(unknown_record_types, 
                                             collapse = ", "))
  }  
    
  #Drop data types not requested by user
  vdat_txt <- vdat_txt[record_type %in% record_types]
  
  #Split into list elements by record type
  vdat_list <- split(vdat_txt, 
                    by = "record_type",
                    keep.by = FALSE)


  # explicitly defines column types
  make_vdat <- function(x){

    header <- data.table::fread(text = paste0(c(x$txt, ""),
                                              collapse = "\n"),
                                sep = ",",
                                na.strings = "",
                                nrows = 1)
    col_types <- col_class_out(names(header))
        
    data.table::fread(text = paste0(c(x$txt,""), 
                                    collapse = "\n"), 
                      sep = ",", na.strings = "",
                      header = TRUE,
                      drop = 1,
                      colClasses = col_types )}
  
  #Read each list element separately
   vdat <- sapply(vdat_list, make_vdat)
  
  #Assign class
  vdat <- structure(vdat, 
                    class = c("vdat_list", class(vdat)),
                    fathom_csv_format_version = src_version$fathom_csv,
                    vdat_version = src_version$vdat_exe)
  
  return(vdat)
}  
  
