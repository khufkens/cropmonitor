#' Start the cropmonitor shiny interface
#' @param img: image to process
#' @keywords region of interest selection
#' @export
#' @examples
#' # no examples yet

estimate.horizon = function(img, plot = FALSE){

  # reads in image or rasterLayer
  # to estimate transitions between
  # land and sky

  # internal function to estimate changepoint values
  # which occur when transitioning from
  # land to sky in the image (the horizon)
  horizon = function(x){
    cpt_obj = changepoint::cpt.mean(
      x * 30,
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
    r = raster::brick(img)

    # in case the image is in portrait mode, transpose and flip
    # to the correct landscape mode
    if (ncol(r) < nrow(r)){
      r = t(raster::flip(r,1))
    }

    # calculate the Gcc values using the
    # second channel of the RGB image (green)
    # and the brightness (the sum of all channels)
    # (this overwrites the original filename)
    img = raster::subset(r,2) / sum(r)
  }

  # if the image file is a 3-layer image
  # calculate the gcc values for breakpoint
  # detection
  if (nlayers(img) == 3){
    img = raster::subset(img,2) / sum(img)
  }

  # in one pass convert the rasterLayer to a matrix
  # and apply() the helper function (estimating the changepoint)
  # to all columns in the matrix
  horizon_locations = apply(as.matrix(img),2,horizon)

  # these are filters to kick out edge values
  # more can be added to refine the algorithm
  horizon_locations[horizon_locations < nrow(img)/2] = NA
  horizon_locations[horizon_locations == 0] = NA
  horizon_locations[horizon_locations > round(nrow(img) * 0.9)] = NA

  # some visual feedback mainly for debugging
  if (plot == TRUE){
    plot(img)
    lines(1:length(horizon_locations),horizon_locations,lwd=2,col='red')
  }

  # return the horizon locations
  return(horizon_locations)
}
