#' Reads and resizes images or raster layers
#' rotates it in the proper direction when necessary
#'
#' @param img: RGB image, or raster file to process
#' @param ncols: resize to a particular number of columns
#' @param nrows: resize to a particular number of rows,
#' if NULL the same ratio is used as set by the original ncol
#' vs. the specified ncols.
#' @keywords image preparation, image reading
#' @export
#' @examples

read_size = function(img,
                     ncols = 640,
                     nrows = NULL){
  
  # resizing helper function
  resize = function(...){
    
    # return image if alread the right size
    if (ncol(img) == ncols){
      return(img)
    }
    
    # calculate number of rows
    if (is.null(nrows)){
      nrows = round(ncols * (nrow(img) / ncol(img)))
    }
    
    # setup reference image
    reference = raster(nrows = nrows,
                       ncols = ncols)
    crs(reference) = NA
    
    # set new extent
    extent(reference) = extent(img)
    
    # resample based upon reference image
    img_resampled = resample(img, reference)
    extent(img_resampled) = c(0,ncol(img_resampled),0,nrow(img_resampled))

    # return the image
    return(img_resampled)
  }
  
  # verify data formats if not transform
  # to the correct data format
  if (class(img) == "character"){
    img = brick(img)
  }
  
  # resize the original image temporarily
  # to determine it's proper orientation
  img_tmp = resize(img = img,
                   ncols = 400,
                   nrows = NULL)
  
  # if possible use the bcc index
  # to find the horizon, distant pixels
  # or sky in general are always more blue
  # than those closer to the camera or containing
  # non vegetative or soil components
  if (nlayers(img_tmp) == 3){
    img_tmp = img_tmp[[3]] / sum(img_tmp)
  }
  
  # fewer breakpoints will be detected on
  # axis parallel to the horizon use
  # the resized image fore
  normal = estimate_horizon(img_tmp)
  rot = estimate_horizon(raster::t(img_tmp))
  
  # ratio of NA values over all values
  # high ratio == lots of missing data
  # indicates wrong axis
  normal_ratio = length(which(is.na(normal)))/length(normal)
  rot_ratio = length(which(is.na(rot)))/length(rot)
  
  # grab the mean horizon location
  hor_loc_normal = mean(normal, na.rm = TRUE)
  hor_loc_rot = mean(rot, na.rm = TRUE)
  
  if (normal_ratio > rot_ratio){
    
    # transpose the original image
    img = raster::t(img)
    img_tmp = raster::t(img_tmp)
    
    # check if the sky is up or not
    sky_val = diff(c(mean(img_tmp[1:hor_loc_rot,]),
                     mean(img_tmp[hor_loc_rot:nrow(img_tmp),])))
    
    # use cols instead of rows as criterium
    # in order not to transpose the image again
    if (sky_val > 0){
      img = resize(img = img,
                   ncols = ncols,
                   nrows = NULL)
      img = flip(img, direction = "y")
      degrees = 270
    } else {
      img = resize(img = img,
                   ncols = ncols,
                   nrows = NULL)
      img = flip(img, direction = "x")
      degrees = 90
    }
  } else {
    
    # check if the sky is up or not
    sky_val = diff(c(mean(img_tmp[1:hor_loc_normal,]),
                     mean(img_tmp[hor_loc_normal:nrow(img_tmp),])))
    
    if (sky_val > 0){
      img = resize(img = img,
                   ncols = ncols,
                   nrows = NULL)
      img = flip(img, direction = "y")
      img = flip(img, direction = "x")
      degrees = 180
    } else {
      img = resize(img = img,
                   ncols = ncols,
                   nrows = NULL)
      degrees = 0
    }
  }

  # clean up raster files
  removeTmpFiles(h=0)
  
  # return resized flipped image
  return(list("img" = img,
              "rotation" = degrees))
}


