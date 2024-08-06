#' An R package for the QA/QC of vrl files downloaded from InnovaSea receivers
#'
#' @description
#' \code{glatosQAQC} is an R package that interactively checks validity of receiver log files (*.vrl) offloaded from InnovaSea acoustic receivers.  A graphical interface is used to select and display summary data from receiver log files (*.vrl) obtained from InnovaSea acoustic receivers.  Because receiver log files are proprietary, receiver data must be extracted from proprietary format using a system call referencing InnovaSea software installed with Fathom Connect.  Extraction of necessary data from log fies is completed automatically when receiver log files are submitted to \code{glatosQAQC} within the graphical user interface, provided Fathom Connect software \url{https://support.fishtracking.innovasea.com/s/downloads} is installed on the computer.
#'
#' @section Package status:
#' This package is in early development. 
#' If you encounter problems or have questions or suggestions, please post an 
#' issue at \url{github.com/haydento/glatosQAQC} or email thayden@usgs.gov (maintainer: Todd Hayden).
#'
#' @section Installation:
#' Package is hosted on GitHub and can be installed automatically via \code{install_github} function in the \code{remotes} package.
#' If you do not have \code{devtools} installed on your machine, first run \code{install.packages("remotes")} in a R terminal.  After package \code{remotes} is installed, run \code{devtools::install_github("haydento/glatosQAQC")} to install \code{glatosQAQC}.
#'
#' \emph{A recent version of Fathom Connect software (Innovasea) must be installed for \code{glatosQAQC} to work.  Fathom Connect can be downloaded at \url{https://support.fishtracking.innovasea.com/s/downloads} 
#'
#' 
#' @section Typical Use:
#' \cr
#' \emph{\code{glatosQAQC} consists of an interactive GUI that enables the user to open and extract summary data from one or more acoustic receiver log files (*.vrl) stored on your computer.}
#' To use run the following code in a R terminal:   
#' 1. \code{library(glatosQAQC)}
#' \cr
#' 2. \code{QAQCapp()}
#' \cr
#' \cr
#' This will open a separate window in your default browser that allows selection of .vrl files.  Once files are selected, a summary table will be generated.
#' 
#' 
#' @docType package
#' @name glatosQAQC
#' @import data.table shiny
globalVariables(".")

#' .onAttach("package", "glatosQAQC")

#package startup message
.onAttach <- function(libname, pkgname) {

  
  vdat <- get_local_vdat_version(vdat_exe_path = NULL)
  min_vdat = "3.4.0"

  if(utils::compareVersion(vdat$version, min_vdat) >= 0){
    m <- paste0("glatosQAQC version ", utils::packageVersion("glatosQAQC"), " ('gen3 Lake Trout')", ", vdat.exe version found: ", vdat$version)
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



