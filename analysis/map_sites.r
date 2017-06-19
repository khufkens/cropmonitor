# visualize acquisitions
# 
# #generate_map = function(){
# library(leaflet)
# library(mapview)
# library(sp)
# #library(sf)
# 
# #path = "/data/Dropbox/Research_Projects/IFPRI/code/cropmonitor/inst/shiny/cropmonitor/"
# 
# df = jsonlite::fromJSON("~/cropmonitor/cropmonitor.json")
# df = df[!is.na(df$gcc),]
# 
# thumbs = sprintf("~/cropmonitor/thumbs/%s/%s/%s-%s-%s.jpg",
#         df$uniqueuserid,
#         df$uniquecropsiteid,
#         df$uniqueuserid,
#         df$uniquecropsiteid,
#         df$pic_timestamp)
# 
# latitude = as.vector(by(df$latitude, INDICES = df$uniquecropsiteid, mean))
# longitude = as.vector(by(df$longitude, INDICES = df$uniquecropsiteid, mean))
# field = as.vector(by(df$uniquecropsiteid, INDICES = df$uniquecropsiteid, function(x) as.character(x[1]) ))
# image_id = as.vector(by(thumbs, INDICES = df$uniquecropsiteid, function(x) as.character(x[1]) ))
# user = as.vector(by(df$uniqueuserid, INDICES = df$uniquecropsiteid, mean))
# 
# d = unlist(lapply(as.vector(image_id),file.exists))
# latitude = latitude[d]
# longitude = longitude[d]
# image_id = image_id[d]
# field = field[d]
# 
# lat_lon = CRS("+init=epsg:4326")
# test = SpatialPointsDataFrame(
#   coords = cbind(longitude,latitude),
#   proj4string = lat_lon,
#   data = data.frame(field, image_id))
# 
# mapview(test,
#         popup = popupImage(as.vector(image_id)),
#         homebutton = FALSE,
#         alpha = 0,
#         lwd = 0,
#         label = field,
#         cex = 4,
#         color = "yellow",
#         map.types = "Esri.WorldImagery")
# #}