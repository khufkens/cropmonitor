#' Start the cropmonitor shiny interface
#' @param img: image to process
#' @keywords region of interest selection
#' @export
#' @examples
#' # no examples yet

estimate_horizon = function(img, 
                            plot = FALSE){

  # reads in image or rasterLayer
  # to estimate transitions between
  # land and sky using the blue channel
  # of an RGB image (or a raster layer of choice)

  # internal function to estimate changepoint values
  # which occur when transitioning from
  # land to sky in the image (the horizon)
  horizon = function(x){
    
    cpt_obj = changepoint::cpt.mean(
      x / 100,
      method = 'PELT',
      test.stat = 'Normal',
      param.estimates = TRUE
    )
    horizon = length(x) - min(cpt_obj@cpts)

    # return the value
    return(horizon)
  }

  # verify data formats if not transform
  # to the correct data format
  if (class(img) == "character"){

    # read in the image to estimate the region of interest of
    img = raster::raster(img, 3)
  }

  # if the image file is a 3-layer image
  # calculate the gcc values for breakpoint
  # detection
  if (raster::nlayers(img) == 3){
    img = raster::subset(img,3) # / sum(img)
  }

  # in case the image is in portrait mode, transpose and flip
  # to the correct landscape mode
  if (ncol(img) < nrow(img)){
    img = t(raster::flip(img,1))
  }
  
  # in one pass convert the rasterLayer to a matrix
  # and apply() the helper function (estimating the changepoint)
  # to all columns in the matrix
  horizon_locations = apply(as.matrix(img),2,horizon)
  
  # these are filters to kick out edge values
  # more can be added to refine the algorithm
  # (remove columns with no obvious breakpoint)
  horizon_locations[horizon_locations == 0] = NA
  horizon_locations[horizon_locations == nrow(img)] = NA

  # some visual feedback mainly for debugging
  if (plot == TRUE){
    plot(img)
    lines(1:length(horizon_locations),horizon_locations,lwd=2,col='red')
  }

  # return the horizon locations
  return(horizon_locations)
}
