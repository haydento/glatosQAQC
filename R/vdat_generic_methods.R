#' @include classes.R
#' Method for custom print of vdat object

# Create "tableizer" class.  Input must be a data.table, cannot be empty.
# create and validate a list of vdats produced by shiny
# @name vdat class print method
# @title print vdat objects
# @param x An S7::vdat object
# @param ... other values

# Create custom print method for vdat object
# this adds on to existing "print" classes in R
S7::method(print, vdat) <- function(x, ...){
  cat("--- vdat ---\n")
  cat("input files:", x@fls, "\n\n")
  cat("total tables:", length(x@rec_data), "\n\n")

  print(x@rec_data)

  return(invisible(x))
}
