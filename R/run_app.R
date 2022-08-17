
#' @title start receiver QAQC app
#' @description Helper function that starts receiver download QAQC app.  This function does nothing else.
#' @export

QAQCapp <- function() {
  appDir <- system.file("shiny", "QAQCapp", package = "glatosQAQC")
  if (appDir == "") {
    stop("Could not find app. Try reinstalling `glatosQAQC`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
