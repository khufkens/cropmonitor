#' Reads and resizes images or raster layers
#' rotates it in the proper direction when necessary
#'
#' @param img: RGB image, or raster file to process
#' @keywords image preparation, image reading
#' @export
#' @examples

read_size = function(img,
                    ncols = 640,
                    nrows = NULL){
  
  resize = function(...){
    
    if (is.null(nrows)){
      ratio = ncol(img)/ncols
      nrows = round(nrow(img) / ratio)
    }
    
    # setup reference image
    reference = raster(nrows = nrows,
                       ncols = ncols)
    extent(reference) = extent(img)
    
    # resample based upon reference image
    img_resampled = resample(img, reference)
    extent(img_resampled) = c(0,ncols,0,nrows)
    
    return(img_resampled)
  }
  
  # verify data formats if not transform
  # to the correct data format
  if (class(img) == "character"){
    img = raster::brick(img)
  }
  
  # resize the original image temporarily
  img_tmp = resize(img)
  
  # if possible use the Gcc index
  # to find the horizon
  if (nlayers(img_tmp) == 3){
    img_tmp = img_tmp[[2]]/sum(img_tmp)
  }
  
  # fewer breakpoints will be detected on
  # axis parallel to the horizon
  normal = estimate_horizon(img_tmp[[1]])
  rot = estimate_horizon(t(img_tmp[[1]]))
  
  normal_ratio = length(which(is.na(normal)))/length(normal)
  rot_ratio = length(which(is.na(rot)))/length(rot)
  
  if (normal_ratio > rot_ratio){
    
    # transpose image
    img_tmp = t(img_tmp)
    img = t(img)
    
    # now check if the horizon is up
    hor_loc = mean(estimate_horizon(img_tmp), na.rm = TRUE)
    print(hor_loc)
    
    if (hor_loc < 0.5 * nrow(img_tmp)){
      img = flip(img, direction = "y")
      img = resize(img)
    } else {
      img = flip(resize(img), direction = "x")
    }
  } else {
    img = resize(img)
  }
  
  # return resized flipped image
  return(img)
}

bla = readsize("~/Desktop/lodging.jpg")


