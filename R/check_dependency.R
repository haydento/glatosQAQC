#' Check path to Innovasea program VDAT.exe
#' 
#' @param vdat_exe_path The full path to \code{VDAT.exe}. If \code{NULL}
#'  (default) then the path to VDAT.exe must be in the PATH environment variable
#'  of the system. 
#'  
#' @returns Character string with command for calling VDAT.exe via
#'   \code{system2}'s \code{\link{command}} argument.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' #use Windows system PATH variable
#' check_vdat()
#' 
#' 
#' #use path to directory containing VDAT.exe
#' check_vdat(vdat_exe_path = "C:/Program Files/Innovasea/Fathom")
#' 
#'
#' #use full path to VDAT.exe
#' check_vdat(vdat_exe_path = "C:/Program Files/Innovasea/Fathom/VDAT.exe")
#' 
#' 
#' }
#'
#' @export
check_vdat <- function(vdat_exe_path = NULL){
  
  if(is.null(vdat_exe_path)) {
    vdat_cmd <- "VDAT"
    
    if(Sys.which(vdat_cmd) == "") stop("VDAT.exe not found in system PATH ",
                                       "variable.", call. = FALSE)
    
  } else {
  
    #remove VDAT.exe from vdat_exe_path if present
    vdat_exe_dir <- ifelse(grepl("vdat.exe$", 
                                 vdat_exe_path, 
                                 ignore.case = TRUE),
                           dirname(vdat_exe_path),
                           vdat_exe_path)
    
    vdat_exe_file <- file.path(vdat_exe_dir, "VDAT.exe")
    
    # Check path to VDAT.exe
    if(!file.exists(vdat_exe_file)) stop("VDAT.exe not found at specified ",
                                         "path.", call. = FALSE)
    
    vdat_cmd <- vdat_exe_file
    
    # Check if path can be reached via system call
    if(Sys.which(vdat_cmd) == "") stop("VDAT.exe found but could not be ",
                                       "reached via system call.", 
                                       call. = FALSE)
  }
  
  return(vdat_cmd)
}


#' Get version of local installation of Innovasea program VDAT.exe
#' 
#' @param vdat_exe_path The full path to \code{VDAT.exe}. If \code{NULL}
#'  (default) then the path to VDAT.exe must be in the PATH environment variable
#'  of the system. See \code{\link{check_vdat}}.
#'   
#' @returns 
#' A list with \code{version} (version number) and \code{long_version} (full 
#' string returned by VDAT.exe).
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' #use if VDAT.exe in Windows system PATH variable
#' get_local_vdat_version()
#'
#' #or specify path to VDAT.exe
#' get_local_vdat_version(vdat_exe_path = 
#'                         "C:/Program Files/Innovasea/Fathom/VDAT.exe")
#' }
#' 
#' @export

get_local_vdat_version <- function(vdat_exe_path = NULL){
  
  # Check path to vdat.exe and get (valid) command arg for system2 call
  vdat_cmd <- check_vdat(vdat_exe_path)
  
  #invoke VDAT.exe
  vdat_call <- "--version"
  
  vdat_version <- system2(vdat_cmd, vdat_call, stdout = TRUE)

  if(grepl("^vdat-", vdat_version)) {
    ver = strsplit(vdat_version, "-")[[1]][2]
    major = as.numeric(strsplit(ver, "[.]")[[1]][1])
    minor = as.numeric(strsplit(ver, "[.]")[[1]][2])
    patch = as.numeric(strsplit(ver, "[.]")[[1]][3])

    vdat_version_out <- list(version = ver,
                             long_version = vdat_version,
                             major = major,
                             minor = minor,
                             patch = patch)
  } else {
    vdat_version_out <- NULL
  }

  return(vdat_version_out)
}

