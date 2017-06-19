#' Start the cropmonitor shiny interface
#' 
#' @param img: single raster layer to process
#' @param penalty: parameter to set the sensitivity of the changepoint
#' detection used in finding the horizon.
#' @param plot: visualize the horizon on the image
#' @keywords region of interest selection
#' @export
#' @examples
#' # no examples yet

estimate_horizon = function(img,
                            penalty = 0.2,
                            plot = FALSE){

  # internal function to estimate changepoint values
  # which occur when transitioning from
  # land to sky in the image (the horizon)
  horizon = function(x, ...){
    
    cpt_obj = changepoint::cpt.mean(
      x,
      method = 'PELT',
      test.stat = 'Normal',
      penalty = "Manual",
      pen.value = penalty,
      param.estimates = TRUE
    )
    horizon = length(x) - min(cpt_obj@cpts)

    # return the value
    return(horizon)
  }

  if (!grepl("Raster*",class(img)) | is.character(img)){
    stop("function needs a Raster object as input")
  }
  
  # make a copy of the original image
  # for plotting purposes
  if (plot){
    img_tmp = img
  }
  
  # convert to matrix
  img = raster::as.matrix(img)
  
  # in one pass convert the rasterLayer to a matrix
  # and apply() the helper function (estimating the changepoint)
  # to all columns in the matrix
  horizon_locations = apply(img, 2, horizon, penalty = penalty)
  
  # these are filters to kick out edge values
  # more can be added to refine the algorithm
  # (remove columns with no obvious breakpoint)
  horizon_locations[horizon_locations == 0] = NA
  horizon_locations[horizon_locations == nrow(img)] = NA
  
  # visualize the horizon when required
  if (plot){
    plot(img_tmp)
    lines(1:ncol(img_tmp),
          horizon_locations,
          lwd = 2,
          col='red')
  }
  
  # return the horizon locations
  return(horizon_locations)
}
