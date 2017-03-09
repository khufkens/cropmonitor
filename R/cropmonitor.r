#' Start the cropmonitor shiny interface
#' @param path : a path were the data is stored (default = ~/cropmonitor/)
#' @keywords GUI
#' @export
#' @examples
#' 
#' ## NOT RUN
#' 
#' cropmonitor()
#' 
#' ## END NOT RUN

cropmonitor = function(path = "~/cropmonitor"){
  
  if ( !dir.exists(path) ){
    stop("no data has been processed yet!\n Please update the database first.")
  }

  if ( !file.exists(sprintf("%s/cropmonitor.json",path)) ) {
    stop("no local database exists, please calculate the data first!")
  }
  
  appDir = sprintf("%s/shiny/cropmonitor",path.package("cropmonitor"))
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}
