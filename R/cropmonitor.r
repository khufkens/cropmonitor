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

cropmonitor = function(path = "~/cropmonitor",
                       rdsfile = "cropmonitor_merged.rds"){
  
  if ( !dir.exists(path) ){
    stop("no data has been processed yet!\n 
         Please update the image database first using update_image_db()")
  }

  if ( !file.exists(sprintf("%s/%s",path, rdsfile)) ){
    stop("no local database exists, please calculate the data first using\n
         process_image_db()")
  }
  
  # start application
  appDir = sprintf("%s/shiny/cropmonitor",path.package("cropmonitor"))
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}
