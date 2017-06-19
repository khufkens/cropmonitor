#' Estimates glcm values for a region of interest
#'
#' @param img: RGB image to process (filename or 3-layer RGB stack or brick)
#' @param plot: plot resulting image with all available information
#' @keywords glcm, feature, entropy
#' @export
#' @examples
#' # no examples yet

calculate_glcm = function(img,
                          roi = NULL){

  # verify data formats if not transform
  # to the correct data format
  if (class(img) == "character"){
    img = raster::brick(img)
  }

  # if no roi is specified calculate the roi
  if (is.null(roi)){
    roi = estimate_roi(img)
  }
  
  # calculate various glcm indices on the Green channel
  if (nlayers(img) == 3){
    img = img[[2]] #/ sum(img)
  } else {
    img = img
  }
  
  # select the ROI from the original image
  img_region = mask(img, roi$roi)
  
  #plot(img_region)
  #lines(roi$roi)
  
  # fix windows relative size based upon the size of the 
  # image to process, this to standardize the metrics across
  # various image inputs
  
  glcm_data = glcm(img_region,
                   window = c(5,5),
                   statistics = c("variance", "homogeneity", "contrast",
                                  "dissimilarity", "entropy", "second_moment"))
  
  # calculate stats across region of interest
  img_region_stats = cellStats(glcm_data, mean, na.rm = TRUE)
  
  # return values as a structure list
  return(list("roi" = roi$roi,
              "horizon" = roi$horizon,
              "glcm" = img_region_stats))
}
