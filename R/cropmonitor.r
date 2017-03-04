#' Start the cropmonitor shiny interface
#' @keywords GUI
#' @export
#' @examples
#' cropmonitor()

ameriflux.explorer <- function(){
  appDir = sprintf("%s/shiny/cropmonitor.r",path.package("cropmonitor"))
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}
