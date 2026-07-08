# Check path to Innovasea program VDAT.exe
#' 
# vdat_exe_path The full path to \code{VDAT.exe}. If \code{NULL}
#  (default) then the path to VDAT.exe must be in the PATH environment variable
#  of the system. 
#  

vdat_check <- function(vdat_exe_path = NULL){

  # common paths to vdat.exe that cover mac/linux/windows
  pths <- c("/home/thayden/.local/bin/vdat.exe",
            "C:/Program Files/Innovasea/Fathom/vdat.exe",
            "vdat.exe",
            "vdat",
            "~/local/bin/vdat.exe")

  # determine where to find vdat.
  pths <- Sys.which(pths)
  
  if(all(pths == "")) stop("VDAT.exe not found in system PATH ",
                           "variable.", call. = FALSE)
  
  vdat_call <- pths[pths != ""][1]   
  return(vdat_call)
}


# Get version of local installation of Innovasea program VDAT.exe
# 
# vdat_exe_path The full path to \code{VDAT.exe}. If \code{NULL}
#  (default) then the path to VDAT.exe must be in the PATH environment variable
#  of the system. See \code{\link{check_vdat}}.
#   
# 
# A list with \code{version} (version number) and \code{long_version} (full 
# string returned by VDAT.exe).
# 
# 
# 

# 
# #use if VDAT.exe in Windows system PATH variable
# vdat_version()
#
# #or specify path to VDAT.exe
# vdat_version(vdat_exe_path = 
#                         "C:/Program Files/Innovasea/Fathom/VDAT.exe")
# }
# 


vdat_version <- function(vdat_exe_path = NULL){
  
  # Check path to vdat.exe and get (valid) command arg for system call
  vdat_cmd <- vdat_check(vdat_exe_path)
  
  #invoke VDAT.exe
  vdat_call <- "--version"

  vdat_version <- processx::run(command = vdat_cmd, args = c(vdat_call))$stdout

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


# extracts vrl files with wine emiluator behind the scenes.  This makes it possible to develop on linux!
# 2026-06-29
vdat_linux <- function(exe_path, args_paths = NULL, ...){

  # convert linux path to windows with winepath
  win_args <- character(0)

  if(!is.null(args_paths)){
    win_args <- sapply(args_paths, function(p) {
      res <- processx::run("winepath", c("-w", p))
      trimws(res$stdout)
    }, USE.NAMES = FALSE)
  }

  processx::run(
    command = "wine",
    args = c(exe_path,
             "convert",
             "--format=csv.fathom",
             "--timec=default",
             "--progress",
             win_args),
    ...
  )
}

