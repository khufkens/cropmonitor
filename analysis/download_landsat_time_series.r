# download all Landsat data using the GEE subset script

# change this depending on system settings
python_path = "/usr/local/bin/python/"

# clone the gee_subset project
# relies on git being installed
# and will work out of the box for most
# on OSX or Linux.
#
# basic gee_subset requirements apply
# mainly, having a working GEE python API install
setwd("~")
system("git clone https://github.com/khufkens/google_earth_engine_subsets.git")
path = "~/google_earth_engine_subsets/gee_subset/"

# local R gee_subset function, calls python module
gee_subset = function(product,
                      band,
                      start_date,
                      end_date,
                      latitude,
                      longitude,
                      scale = 30) {
  
  # grab tempdir location
  directory = tempdir()
  
  # download the data and time
  system(
    sprintf(
      "%s %s/gee_subset.py -p %s -b %s -s %s -e %s -l %s %s -d %s -sc %s",
      python_path,
      path,
      product,
      band,
      start_date,
      end_date,
      latitude,
      longitude,
      directory,
      scale
    ),
    wait = TRUE,
    ignore.stdout = TRUE
  )
  
  # read in the data stored in the temporary directory
  df = read.table(sprintf("%s/site_gee_subset.csv", directory),
                  sep = ",",
                  header = TRUE)
  
  # remove file from disk
  file.remove(sprintf("%s/site_gee_subset.csv", directory))
  
  # convert to proper date format
  df$date = as.Date(df$date)
  
  # order data for convenience
  df = df[order(df$date),]
  
  # return the data
  return(df)
}

# store output in the R temporary directory
directory = tempdir()

# read site data
df = readRDS("~/cropmonitor/cropmonitor.rds")

# create unique field vector
df$userfield = paste(df$uniqueuserid,df$uniquecropsiteid,sep = "-")

# summarize variables
latitude = as.vector(by(df$latitude,
                        INDICES = df$userfield,
                        function(x)x[1]))

# add 15m to latitude (move north)
offset = (0.00001/1.1132) * 15
latitude = latitude + offset

longitude = as.vector(by(df$longitude,
                         INDICES = df$userfield,
                         function(x)x[1]))

field = as.vector(by(df$userfield,
                     INDICES = df$userfield,
                     function(x) as.character(x[1])))

# count the images
image_count = table(df$userfield)
image_count = data.frame(as.character(names(image_count)),as.numeric(image_count))
colnames(image_count) = c('field','images')

# group map data
loc = data.frame(field, latitude, longitude)
loc = merge(loc, image_count, by = 'field', all.x = TRUE)
loc = loc[order(loc$images, decreasing = TRUE),]

# set date range, fixed for all products
start_date = "2016-10-15"
end_date = "2017-05-15"

# get nr fields
l = nrow(loc)

pb = txtProgressBar(min = 1, max = l, style = 3)
# loop over all data and download the time series
#for (i in 1:length(field)){
tier1 = lapply(1:3, function(i){

  # set progress
  setTxtProgressBar(pb, i)
  
  # set product parameters, such as
  # product name, band(s) to query, start and end date of the range
  # and the lcoation
  df = gee_subset(product = "LANDSAT/LC08/C01/T1",
                  band = "B2 B4 B5 BQA",
                  start_date = start_date,
                  end_date = end_date,
                  latitude = loc$latitude[i],
                  longitude = loc$longitude[i],
                  scale = 30)
  
  # return data
  return(df)
})
close(pb)

pb = txtProgressBar(min = 1, max = l, style = 3)
# loop over all data and download the time series
#for (i in 1:length(field)){
BAI = lapply(1:3, function(i){
  
  # set progress
  setTxtProgressBar(pb, i)
  
  # set product parameters, such as
  # product name, band(s) to query, start and end date of the range
  # and the lcoation
  df = gee_subset(product = "LANDSAT/LC8_L1T_8DAY_BAI",
                  band = "BAI",
                  start_date = start_date,
                  end_date = end_date,
                  latitude = loc$latitude[i],
                  longitude = loc$longitude[i],
                  scale = 30)
  
  # return data
  return(df)
})
close(pb)