#' Estimates the gcc value for a region of interest
#' automatically generated by any given algorithm
#'
#' @param img: RGB image to process (filename or 3-layer RGB stack or brick)
#' @param roi: existing roi description image
#' @param plot: plot resulting image with all available information
#' @keywords gcc calculation
#' @export
#' @examples
#' # no examples yet

calculate_gcc = function(img,
                         roi = NULL,
                         plot = FALSE){

  # set default file type
  file_type = "img"

  # verify data formats if not transform
  # to the correct data format
  if (class(img) == "character"){

    # set file type
    file_type = "file"

    # read in the image to estimate the region of interest of
    img = raster::brick(img)
  }

  # additional check if the stack / brick has three layers
  if (raster::nlayers(img) != 3){
    stop("the raster object does not have the required 3 (RGB) layers!")
  }

  # if no roi is specified calcualte the roi
  if (is.null(roi)){
    # estimate an ROI
    roi = estimate_roi(img, plot = plot)
  }
  
  # select the ROI from the original image
  img_region = raster::mask(img, roi$roi)
  
  # calculate various indices
  gcc = raster::subset(img_region,2) / sum(img_region)
  grvi = (raster::subset(img_region,2) -  raster::subset(img_region,1)) /
    (raster::subset(img_region,2) + raster::subset(img_region,1))

  # gcc 90 (90th percentile)
  gcc_90 = quantile(gcc, 0.9)

  # mean colour values
  r_dn = mean(raster::subset(img_region,1))
  g_dn = mean(raster::subset(img_region,2))
  b_dn = mean(raster::subset(img_region,3))
  
  # GRVI (10th percentile)
  grvi_10 = quantile(grvi, 0.1)

  # return values as a structure list
  return(list("roi" = roi$roi,
              "horizon" = roi$horizon,
              "gcc" = gcc_90,
              "grvi" = grvi_10,
              "r_dn" = r_dn,
              "g_dn" = g_dn,
              "b_dn" = b_dn))
}
