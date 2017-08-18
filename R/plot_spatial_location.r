#' Plot the spatial location of a PBI farmer
#' to determine if the geographic location
#' of the time series corresponds with a field
#' on the ground. Used in QA/QC for satellite
#' data comparisons.
#'
#' @param df: cropmonitor database dataframe
#' @param userfield: userfield, concated userid and field # as in user-field
#' @param path: path location where to output summary graphics
#' @keywords QA/QC
#' @export
#' @examples
#' # no examples yet
#' \dontrun{
#' # Load main data file, including all indices
#' df = readRDS("~/cropmonitor/cropmonitor.rds")
#'
#' # screen for test sites in the US / EU
#' df = df[which(df$longitude > 70),]
#' df = df[which(as.Date(df$date) > as.Date("2016-10-01")),]
#'
#' # create unique field vector
#' df$userfield = paste(df$uniqueuserid,df$uniquecropsiteid,sep = "-")
#' 
#' # plot spatial location graph in home directory
#' plot_spatial_location(df, userfield = 430720-1, path = '~')
#' 
#' }

library(dplyr)
library(ggplot2)
library(gridExtra)

plot_spatial_location = function(df = df,
                                 userfield = NULL,
                                 path = "~"){
  
  if(is.null(userfield) || !is.character(userfield) ){
    stop("Please provide a userfield as a character string!")
  }
  
  # subset the original data in the function
  df = df[which(df$userfield == userfield),]
  
  # generate strings for thumbs
  df$thumbs = sprintf("~/cropmonitor/thumbs/%s/%s/%s-%s-%s.jpg",
                      df$old_uniqueuserid,
                      df$old_uniquecropsiteid,
                      df$old_uniqueuserid,
                      df$old_uniquecropsiteid,
                      df$pic_timestamp)
  
  # check if files exist on disk
  df$thumbs_exist = unlist(lapply(df$thumbs, file.exists))
  
  # select a thumbnail which exists for plotting
  thumbnail = df$thumbs[which(df$thumbs_exist & !is.na(df$thumbs))][1]
  
  # download google maps data
  mymap = try(ggmap::get_map(location = c(lon = as.numeric(df$longitude[1]),
                                      lat = as.numeric(df$latitude[1])),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 18), silent = TRUE)
  
  if (inherits(mymap,"try-error")){
    stop("Server is unreachable or too many requests made!")
  }
  
  # set point location
  point_loc = data.frame(lon = as.numeric(df$longitude[1]),
                         lat = as.numeric(df$latitude[1]))
  
  # plot the google maps data
  p = ggmap::ggmap(mymap,
                   extent = "device",
                   ylab = "latitude",
                   xlab = "longitude") +
    geom_point(data = point_loc,
               aes(x = lon,
                   y = lat),
               color = "red",
               size = 3) +
    labs(title = sprintf("User-Field: %s",userfield),
         fill = "") +
    theme(title = element_text(size = 18))
  
  # plot the thumbnail
  if (file.exists(thumbnail)){
    img = jpeg::readJPEG(thumbnail)
    
    pp = ggplot(data.frame()) +
      annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) +
      stat_bin2d() +
      scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
  } else {
    pp = ggplot(data.frame())
  }
  
  # print final layout to file
  png(sprintf("%s/%s.png", path, userfield), 900, 450)
    grid.arrange(p,pp, ncol = 2)
  dev.off()
}

plot_spatial_location(df, "430720-1")
