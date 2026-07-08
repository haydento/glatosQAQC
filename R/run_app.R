#' @title
#' Start the glatosQAQC app.
#'
#' @description
#' This function starts the interactive web app built by this package.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'
#' @details
#' This is a basic function which does only one thing: starts the interactive app.  There are no input arguments.
#'
#' @return Interactive application object app.
#' 
#' @export
#'
#' @examples
#' if(interactive()){
#' QAQCapp()
#' }

QAQCapp <- function(...) {

  # get original size of memory allocated to shiny app
  old_size <- getOption("shiny.maxRequestSize")

  on.exit(options(shiny.maxRequestSize = old_size))
  
  # set package specific size
  options(shiny.maxRequestSize = 1000 * 1024^2) # enables file size up to 1GB
  
  
  # kick off app
  shiny::shinyApp(
    ui = QAQC_ui(),
    server = QAQC_server
  )
}

  
