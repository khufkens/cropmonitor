#' Plot the spatial location of a PBI farmer
#' to determine if the geographic location
#' of the time series corresponds with a field
#' on the ground. Used in QA/QC for satellite
#' data comparisons.
#'
#' @param df cropmonitor database dataframe
#' @param userfield userfield, concated userid and field # as in user-field
#' @param path path location where to output summary graphics
#' @param stack_path path to subsets of the HLS images, to map the grid of the
#' harmonized landsat sentinel data
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

plot_spatial_location = function(df = df,
                                 userfield = NULL,
                                 path = "~",
                                 stack_path = "~"){
  
  if(is.null(userfield) || !is.character(userfield) ){
    stop("Please provide a userfield as a character string!")
  }
  
  # create unique field vector
  df$userfield = paste(df$uniqueuserid,df$uniquecropsiteid,sep = "-")
  
  # subset the original data in the function
  df = df[which(df$userfield == userfield),]
  
  # get tif file which matches the userfield
  tif_files = list.files(path = stack_path, "*.tif$", full.names = TRUE)
  tif_file = tif_files[grepl(userfield, tif_files)]
  
  # generate strings for thumbs
  df$thumbs = sprintf("~/cropmonitor/thumbs/%s/%s/%s-%s-%s.jpg",
                      df$old_uniqueuserid,
                      df$old_uniquecropsiteid,
                      df$old_uniqueuserid,
                      df$old_uniquecropsiteid,
                      df$pic_timestamp)
  
  # grab median location
  lon = median(as.numeric(df$longitude), na.rm = TRUE)
  lat = median(as.numeric(df$latitude), na.rm = TRUE)
  lat_off = median(as.numeric(df$latitude_offset), na.rm = TRUE)
  
  # check if files exist on disk
  df$thumbs_exist = unlist(lapply(df$thumbs, file.exists))
  
  # select a thumbnail which exists for plotting
  thumbnail = df$thumbs[which(df$thumbs_exist & !is.na(df$thumbs))][1]
  
  # download google maps data
  mymap = try(ggmap::get_map(location = c(lon = lon,
                                      lat = lat),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 17.5), silent = TRUE)
  
  if (inherits(mymap,"try-error")){
    stop("Server is unreachable or too many requests made!")
  }
  
  # set point location
  point_loc = data.frame(lon = lon,
                         lat = lat,
                         lat_off = lat_off)
  
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
    geom_segment(data = point_loc, aes(x=lon, xend=lon, y=lat, yend=lat_off), 
                 arrow = arrow(length = unit(0.5, "cm")),
                 colour = "red") +
    labs(title = sprintf("User-Field: %s",userfield),
         fill = "") +
    theme(title = element_text(size = 18))
  
  if(length(tif_file) != 0){
    
    # test
    print(tif_file)
    
    # read in the raster file and extract the grid
    r = raster::rasterToPolygons(raster::raster(tif_file))
    
    # reproject
    r = sp::spTransform(r, CRS("+init=epsg:4326"))
    p = p + geom_polygon(data = fortify(r),
                                          aes(long,lat,group=group),
                                          colour = "yellow",
                                          fill = NA) +
      geom_point(data = point_loc,
                 aes(x = lon,
                     y = lat_off),
                 color = "red",
                 size = 3,
                 shape = 3)
  }
  
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