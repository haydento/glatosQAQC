#' @export
QAQCapp <- function() {
  appDir <- system.file("shiny", "QAQCapp", package = "glatosQAQC")
  if (appDir == "") {
    stop("Could not find app. Try reinstalling `glatosQAQC`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
