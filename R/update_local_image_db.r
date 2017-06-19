#' Batch download, resize and reorient all data
#' in the IFPRI database using the
#' automatic horizon and ROI detection methods
#'
#' @param database: STATA file as provided by IFPRI
#' @param path: path of the IFPRI database images to process
#' @param server: location of the images online
#' including estimating the horizon and ROI, as well as indices / features
#' @keywords gcc calculation, QA/GC
#' @export

update_local_image_db = function(database = NULL,
                                 path = "~/cropmonitor",
                                 server = "http://cdn.wheatcam.ifpri.org/ReportImages"){

  # for consistency with list.files() use tilde
  # expansion if applicable
  path = path.expand(path = path)
  
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
  
  # sort by user id
  df = df[order(df$old_uniqueuserid),]
  
  # check which files exist in the current directory
  # split out some variable for clarity
  userid = df$old_uniqueuserid
  reportid = df$reportid
  cropsite = df$old_uniquecropsiteid
  epoch_date = df$pic_timestamp
  
  user_img_path = paste(img_path, userid, sep = "/")
  user_thumb_path = paste(thumb_path, userid, sep = "/")
  site_user_img_path = paste(user_img_path, cropsite, sep = "/")
  site_user_thumb_path = paste(user_thumb_path, cropsite, sep = "/")
  
  # create image location string
  filename = paste(userid,"-",
                     cropsite,"-",
                     epoch_date,".jpg", sep = "")
  
  online_image_location = paste(server,
                                userid,
                                cropsite,
                                filename,
                                sep = "/")
  
  local_image_location = paste(site_user_img_path,
                               filename,
                               sep = "/")
  
  thumbnail_location = paste(site_user_thumb_path,
                             filename,
                             sep = "/")
  
  current_image_location = list.files(img_path,
                                      "*.jpg",
                                      recursive = TRUE,
                                      full.names = TRUE)
  
  # list the index locations of files which are missing on disk
  files_to_process = which(!local_image_location %in% current_image_location)
  
  # setup folder locations to be filled with data
  # recursively loop over all unique folders, by user
  # then by user and by site (for both images and thumbnails)
  
  # create folders based on the farmer's id
  for (i in unique(user_img_path)){
    if (!dir.exists(i) ){
      dir.create(i)  
    }
  }
  
  # create folders based on the farmer's id
  for (i in unique(site_user_img_path)){
    if (!dir.exists(i)){
      dir.create(i)  
    }
  }
  
  # create folders based on the farmer's id
  for (i in unique(user_thumb_path)){
    if (!dir.exists(i) ){
      dir.create(i)  
    }
  }
  
  # and his different fields (if any)
  for (i in unique(site_user_thumb_path)){
    if (!dir.exists(i) ){
      dir.create(i)  
    }
  }
  
  # now loop over all new images in the database and fill in the
  # missing values. Report progress
  cat(sprintf("Processing %s database entries\n", length(files_to_process)))
  cat("or updating the existing database.\n")

  # create a progress bar
  pb = txtProgressBar(min = 0, max = nrow(df), style = 3)
  k = nrow(df) - length(files_to_process)
  
  for (i in files_to_process){
      
      # set progress bar
      setTxtProgressBar(pb, k)
      k = k + 1
    
      # set raster options
      rasterOptions(overwrite = TRUE)                                        
                     
      # if there is picture time stamp this is
      # is a free format comment not a picture
      # skip
      if (is.na(epoch_date[i])){
        # clear memory
        gc()
        next
      }
                     
      # download the image data if no local copy exists
      if (!file.exists(local_image_location[i])){
        error = try(curl::curl_download(online_image_location[i],
                                        local_image_location[i],
                                        quiet = TRUE))
        if (inherits(error,"try-error")){
          # return empty values for those which are normally returned
          print("failed download")
          return(NA)
        }
      }
      
      # on linux resize using imagemagick for speed
      # on Windows things will be very slow
      # make sure to use brew to install imagemagick on OSX
      if(.Platform$OS.type == "unix") {
        system(sprintf('mogrify -resize "640x" %s',local_image_location[i]),
             wait = TRUE)
      }
 
      # read a temporary image and resize,
      # flip in the correct direction
      rotated_img = try(read_size(local_image_location[i]))
      
      # skip value if rotating the image fails
      if(inherits(rotated_img,"try-error")){
        # clear memory
        gc()
        next
      }
      
      # estimate an ROI (based upon the location of the horizon)
      roi = suppressWarnings(estimate_roi(rotated_img$img, plot = FALSE))
      
      # write to file using jpeg library (overwrite the original)
      jpeg::writeJPEG(raster::as.array(rotated_img$img/255),
                        target = local_image_location[i],
                        quality = 1)
      
      # write thumbnail to disk
      write_thumbnail(img = rotated_img$img,
                      roi = roi,
                      path = thumbnail_location[i])
      
      # clear memory
      gc()
  }
  
  # close progress bar
  close(pb)
}