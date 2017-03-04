#' Estimates a region of interest (ROI) using the
#' approximate horizon location in an image.
#'
#' This method works auto-magically but might
#' fail in some instances. As such there is a
#' plotting and output function to visualize the
#' actual ROI for quality control
#'
#' @param img: RGB image to process (filename or 3-layer RGB stack or brick)
#' @param padding: % of the image width / height to pad
#' @param plot: plot resulting image with all available information
#' @keywords region of interest selection
#' @export
#' @examples
#' # no examples yet

estimate.roi = function(img,
                        padding = 0.1,
                        plot = FALSE){

  # set default file type
  file_type = "img"

  # verify data formats if not transform
  # to the correct data format
  if (class(img) == "character"){

    # set file type
    file_type = "file"

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

  # in case the image is in portrait mode, transpose and flip
  # to the correct landscape mode
  if (ncol(img) < nrow(img)){
    img = t(
      raster::flip(img,1)
      )
  }

  # calculate some basic image statistics to be used
  # in further processing
  img_mid = ncol(img)/2
  img_width = ncol(img)
  img_height = nrow(img)

  # padding by 10% (values in pixels)
  padding_y = round(nrow(img) * padding)
  padding_x = round(ncol(img) * padding)

  # estimate the horizon locations
  horizon_locations = estimate.horizon(img)

  # I will split the image in two halfs to estimate the left and right
  # median locations / height of the horizon in the image as to compensate
  # for a slanted horizon (rather common in the images)
  left_horizon_location = median(horizon_locations[1:img_mid],na.rm = TRUE)
  right_horizon_location = median(horizon_locations[img_mid:img_width],na.rm = TRUE)

  # I'm using the padding values and various coordinates to set the final
  # polygon coordinates
  left_x = padding_x
  right_x = ncol(img) - padding_x

  # format the coordinates in x-coordinate and y-coordinate vectors
  # go counter clockwise from the bottom of the image
  x = c(left_x,
        right_x,
        right_x,
        left_x)
  y = c(padding_y,
        padding_y,
        right_horizon_location - padding_y,
        left_horizon_location - padding_y)

  # now convert the raw coordinates into a SpatialPolygons
  # object for easier subsetting or conversion to a raster image
  roi = sp::SpatialPolygons(
    list(sp::Polygons(
      list(sp::Polygon(
        matrix(c(x,y), ncol=2, byrow=FALSE)
        )), "bb")))

  # provide the option of plotting all data for feedback
  # and debugging, and post-processing QA/QC
  if (plot == TRUE){
    if (file_type == "file"){
      plotRGB(r)
    } else {
      plot(img)
    }
    lines(1:ncol(img),
          horizon_locations,
          lwd = 2,
          col='red')
    lines(roi,
          lwd = 2,
          lty = 2,
          col = 'black')
  }

  # return data as a SpatialPolygon object
  # can be converted to matrix or vector if needed
  return("roi"=roi,"horizon"=horizon_locations)
}
