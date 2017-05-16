#' Batch process all data in the IFPRI database using the
#' automatic ROI detection methodology
#'
#' @param database: STATA file as provided by IFPRI
#' @param path: path of the IFPRI database images to process
#' @param plot: TRUE / FALSE (output summary plots?)
#' @param force: force regeneration of all indices
#' @param force_all: force regeneration of all data in database
#' including estimating the horizon and ROI, as well as indices / features
#' @keywords gcc calculation, QA/GC
#' @export
#' @examples
#' # no examples yet

process_cropmonitor_db = function(database = NULL,
                                  path = "~/cropmonitor/",
                                  server = "http://cdn.wheatcam.ifpri.org/ReportImages",
                                  plot = FALSE,
                                  force = FALSE,
                                  force_all = FALSE){

  # create directory to hold all the data
  if (!dir.exists(path)){
    dir.create(path)
  }
  
  # create image and thumb paths
  img_path = sprintf("%s/images",path)
  thumb_path = sprintf("%s/thumbs",path)
  
  # create subdirs holding original images
  # and image thumbnails
  if (!dir.exists(img_path) ){
    dir.create(img_path)  
  }
  if (!dir.exists(thumb_path) ){
    dir.create(thumb_path)
  }
  
  # move into the directory
  setwd(path)
  
  # check if parameters are available
  if ( is.null(database) ){
    stop("Not all required parameters available!")
  }

  # read in database file
  df = readstata13::read.dta13(database)
  
  # create empty data frame objects to populate
  # with new data
  df$gcc = NA
  df$grvi = NA
  df$roi = NA
  
  # compare current local database with the one provided
  # as a parameter
  if (file.exists("cropmonitor.json")){
    
    # read local database
    local_database = jsonlite::fromJSON("cropmonitor.json")
    
    # create unique identifiers to compare files
    old_file_id = sprintf("%s-%s-%s",
                          local_database$uniqueuserid,
                          local_database$uniquecropsiteid,
                          local_database$pic_timestamp)
    
    new_file_id = sprintf("%s-%s-%s",
                          df$uniqueuserid,
                          df$uniquecropsiteid,
                          df$pic_timestamp)
    
    # figure out which lines to progress
    # those in the DTA file but not in the existing JSON one
    files_to_process = which(!new_file_id %in% old_file_id)
    
    # make no assumptions on the location of the columns wiht
    # greenness an other ancillary data, determine dynamically 
    new_cols = which(names(df) == c("gcc","grvi","roi"))
    old_cols = which(names(local_database) == c("gcc","grvi","roi"))
    
    # loop over all data in the old file, and populate
    # the read in DTA file (presumably with extra images added)
    for ( j in 1:length(old_file_id)){
      loc = which(new_file_id %in% old_file_id[j])
      df[loc,new_cols] = local_database[j,old_cols]
    }
    
  } else {
    
    # on clean start, calculate everything
    files_to_process = 1:nrow(df)
  
  }
  
  # now loop over all new images in the database and fill in the
  # missing values. Report progress
  cat("Processing database entries\n")
  cat("or updating the existing database.\n")
  cat("[This could take a while]\n\n")
  pb = utils::txtProgressBar(min = 0, max = max(files_to_process), style = 3)
  
  for ( i in files_to_process ){
    
    # set progressbar
    utils::setTxtProgressBar(pb, i)
    
    # split out some variable for clarity
    userid = df$uniqueuserid[i]
    reportid = df$reportid[i]
    cropsite = df$uniquecropsiteid[i]
    epoch_date = df$pic_timestamp[i]
    
    user_img_path = sprintf("%s/%s",img_path,userid)
    user_thumb_path = sprintf("%s/%s",thumb_path,userid)
    site_user_img_path = sprintf("%s/%s",user_img_path,cropsite)
    site_user_thumb_path = sprintf("%s/%s",user_thumb_path,cropsite)
    
    # create folders based on the farmer's id
    if (!dir.exists(user_img_path) ){
      dir.create(user_img_path)  
    }
    
    # create folders based on the farmer's id
    if (!dir.exists(user_thumb_path) ){
      dir.create(user_thumb_path)  
    }
    
    # create folders based on the farmer's id
    if (!dir.exists(site_user_img_path) ){
      dir.create(site_user_img_path)  
    }
    
    # and his different fields (if any)
    if (!dir.exists(site_user_thumb_path) ){
      dir.create(site_user_thumb_path)
    }
    
    # create image location string
    filename = sprintf("%s-%s-%s.jpg",userid,cropsite,epoch_date)
    online_image_location = sprintf("%s/%s/%s/%s",server,userid,cropsite,filename)
    local_image_location = sprintf("%s/%s",site_user_img_path,filename)
    
    # download the image data if no local copy exists
    if (!file.exists(local_image_location)){
      error = try(download.file(online_image_location, local_image_location, quiet = TRUE))
      if (inherits(error,"try-error")){
        next # skip image if download fails
      }
    }
    
    # check if an ROI exists in the database, if so use this
    # don't recalculate unless the FORCE flag is set.
    
    # calculate the gcc etc values
    values = calculate_gcc(local_image_location)
    
    # visualize the regions of interest and horizon
    # in an image thumbnail for review
    
    # read in the data, and flip image if necessary
    img = raster::brick(local_image_location)
    if (ncol(img) < nrow(img)){
      img = t( raster::flip(img,1) )
    }
    
    # plot and save to file
    thumbnail_location = sprintf("%s/%s",site_user_thumb_path,filename)
    jpeg(thumbnail_location, 374, 280)
    raster::plotRGB(img)
    lines(1:ncol(img),
          values$horizon,
          lwd = 2,
          col='red')
    lines(values$roi,
          lwd = 2,
          lty = 2,
          col = 'yellow')
    dev.off()
    
    # stuff things back into the original dataframe
    # long objects are stored as comma separated strings
    df$gcc[i] = values$gcc
    df$grvi[i] = values$grvi
    df$roi[i] = paste(as.vector(values$roi@polygons[[1]]@Polygons[[1]]@coords),collapse = ',')
    
    # return data fit for parallel processing
    #gcc = values$gcc
    #grvi = values$grvi
    #roi = paste(as.vector(values$roi@polygons[[1]]@Polygons[[1]]@coords),collapse = ',')
    #roi = paste(as.vector(values$roi@polygons[[1]]@Polygons[[1]]@coords),collapse = ',')
    #return(list('gcc' = gcc, 'grvi' = grvi, 'roi' = roi))
    
    # write new data to file
    jsonlite::write_json(df,"cropmonitor.json")
    
  }

  # write new data to file
  jsonlite::write_json(df,"cropmonitor.json")
  
  # close progress bar
  utils::close(pb)
  
}