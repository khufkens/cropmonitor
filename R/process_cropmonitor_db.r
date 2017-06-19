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

process_cropmonitor_db = 
  function(database = NULL,
           path = "~/cropmonitor/",
           server = "http://cdn.wheatcam.ifpri.org/ReportImages",
           thumbnails = FALSE,
           force = FALSE,
           ncores = 6){
  
  # for consistency with list.files() use tilde
  # expansion if applicable
  path = path.expand(path = path)
  
  # check if parameters are available
  if ( is.null(database) ){
    stop('No database file available!')
  }
  
  # create directory to hold all the data
  if (!dir.exists(path)){
    stop('data directory does not exist')
  }
  
  # create image and thumb paths
  img_path = sprintf("%s/images",path)
  thumb_path = sprintf("%s/thumbs",path)
  
  # create subdirs holding original images
  # and image thumbnails
  if (!dir.exists(img_path) ){
    stop('data directory does not exist')  
  }
  
  # move into the directory
  setwd(path)
  
  # read in database file
  # sort by user id
  df = readstata13::read.dta13(database)
  
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
  
  # list the index locations of files which are in the local image db
  files_to_process = which(local_image_location %in% current_image_location)
  
  # compare current local database with the one provided
  # as a parameter
  if (file.exists("cropmonitor.json")){
    
    # read local database
    local_database = jsonlite::fromJSON("cropmonitor.json")
    
    # check if the header is not the same
    # calculate everything regardless
    if(names(df) != names(local_database)){
      break
    }
    
    # create unique identifiers to compare files
    old_file_id = sprintf("%s-%s-%s",
                          local_database$old_uniqueuserid,
                          local_database$old_uniquecropsiteid,
                          local_database$pic_timestamp)
    
    new_file_id = sprintf("%s-%s-%s",
                          df$old_uniqueuserid,
                          df$old_uniquecropsiteid,
                          df$pic_timestamp)
    
    # figure out which lines to progress
    # those in the DTA file but not in the existing JSON one
    duplicates = which(new_file_id %in% old_file_id)
    
    # copy duplicates
    
  }
  
  # now loop over all new images in the database and fill in the
  # missing values. Report progress
  cat("Processing database entries\n")
  cat(sprintf("will finish in ~ %s days on a single core macbook pro.\n",
              round((length(files_to_process) * 13) / (3600 * 24 * ncores),2)))

  # set the number of cores
  cl = makeCluster(ncores)
  registerDoSNOW(cl)
  
  # set progress bar
  pb = txtProgressBar(max = length(files_to_process),
                       style=3)
  progress = function(n) setTxtProgressBar(pb, n)
  opts = list(progress = progress)
  
  # use lapply instead of loop
  crop_index_details = foreach(i = files_to_process[1:5],
                               .combine = rbind,
                               .options.snow = opts) %do% {
    
    # if there is picture time stamp this is
    # is a free format comment not a picture
    # skip
    if (is.na(df$pic_timestamp[i])){
      return(rep(NA,9))
    }
    
    # load image
    img = raster::brick(local_image_location[i])
    
    # estimate an ROI (based upon the location of the horizon)
    roi = try(estimate_roi(img = img, plot = FALSE))
    
    if(inherits(roi,"try-error")){
      return(rep(NA,9))
    }

    # calculate the gcc / grvi
    greenness_values = calculate_gcc(img = img,
                                     roi = roi)
    
    # calculate the glcm
    glcm_values = calculate_glcm(img = img,
                                 roi = roi)
    
    # calculate the sobel data
    sobel_values = calculate_sobel(img = img,
                                   roi = roi)
    
    # visualize the regions of interest and horizon
    # in an image thumbnail for review, requires
    # flag
    if (thumbnails){
      thumbnail_location = sprintf("%s/%s",site_user_thumb_path[i],filename[i])
      jpeg(thumbnail_location, 374, 280, bg = "black")
      raster::plotRGB(img)
      lines(1:ncol(img),
            roi$horizon,
            lwd = 2,
            col='red')
      lines(roi$roi,
            lwd = 2,
            lty = 2,
            col = 'yellow')
      dev.off()
    }
    
    # the original ROI polygon descriptor
    #roi = paste(as.vector(greenness_values$roi@polygons[[1]]@Polygons[[1]]@coords),collapse = ',')

    # garbage collection cleanup
    raster::removeTmpFiles()

    # return values
    return(c(greenness_values$gcc,greenness_values$grvi,glcm_values$glcm,sobel_values$sobel))
  }
  
  # output matrix
  output = matrix(NA, nrow(df), 9)
  output[files_to_process[1:5],] = crop_index_details
  names(output) = c("gcc_90",
                    "grvi",
                    "glcm_variance",
                    "glcm_homogeneity",
                    "glcm_contrast",
                    "glcm_dissimilarity",
                    "glcm_entropy",
                    "glcm_second_moment",
                    "sobel")
  
  # write to file after combining with the
  # original database
  df = cbind(df,output)
  
  # write new data to file
  jsonlite::write_json(df,"cropmonitor.json")
  
  # stop cluster
  stopCluster(cl)
  
  # close progress bar
  close(pb)
}