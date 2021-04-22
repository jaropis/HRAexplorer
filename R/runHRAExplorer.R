#' Function running the application from package
#' @export

runHRAexplorer <- function() {
  appDir <- system.file("shiny", package = "HRAexplorer")
  if (appDir == "") {
    stop("Application directory not found", call. = FALSE)
  }
  shiny::runApp(appDir)
}
