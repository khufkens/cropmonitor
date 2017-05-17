#' Start the cropmonitor shiny interface
#' 
#' @param img: raster layer to process
#' @keywords region of interest selection
#' @export
#' @examples
#' # no examples yet

estimate_horizon = function(img){

  # internal function to estimate changepoint values
  # which occur when transitioning from
  # land to sky in the image (the horizon)
  horizon = function(x){
    
    cpt_obj = changepoint::cpt.mean(
      x,
      method = 'PELT',
      test.stat = 'Normal',
      penalty = "Manual",
      pen.value = 0.5,
      param.estimates = TRUE
    )
    horizon = length(x) - min(cpt_obj@cpts)

    # return the value
    return(horizon)
  }

  if (!grepl("Raster*",class(img)) | is.character(img)){
    stop("function needs a Raster object as input")
  }
  
  # in one pass convert the rasterLayer to a matrix
  # and apply() the helper function (estimating the changepoint)
  # to all columns in the matrix
  horizon_locations = apply(as.matrix(img), 2, horizon)
  
  # these are filters to kick out edge values
  # more can be added to refine the algorithm
  # (remove columns with no obvious breakpoint)
  horizon_locations[horizon_locations == 0] = NA
  horizon_locations[horizon_locations == nrow(img)] = NA

  # return the horizon locations
  return(horizon_locations)
}
