# @section Startup hooks
# Internal functions executed at package startup, loading, and attaching.
# @keywords internal

# avoid R CMD check note
globalVariables(".")

# .onAttach("package", "glatosQAQC")

#package startup message
.onAttach <- function(libname, pkgname) { 
  vdat <- vdat_version(vdat_exe_path = NULL)
  min_vdat = "10.0.0"

  if(utils::compareVersion(vdat$version, min_vdat) >= 0){
    m <- paste0("glatosQAQC version ", utils::packageVersion("glatosQAQC"), " ('walleye')", ", vdat.exe version found: ", vdat$version)
    m <- strwrap(m, width = getOption("width"))
    packageStartupMessage(paste0(m, collapse = "\n"))
  }
  
  if(utils::compareVersion(vdat$version, min_vdat) < 0){
    m <- paste0("glatosQAQC version ", utils::packageVersion("glatosQAQC"), " ('Lake Trout')", ", vdat.exe version found: ", vdat$version)
    m <- strwrap(m, width = getOption("width"))
    packageStartupMessage(" WARNING: VDAT NOT INSTALLED OR OUT OF DATE. glatosQAQC OUTPUT MAY BE INCORRECT!  UPDATE FATHOM BEFORE CONTINUING:")
    packageStartupMessage(paste0(m, collapse = "\n"))
  }

}

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  invisible()
}

