#' Batch process all data in the IFPRI database using the
#' automatic ROI detection methodology
#'
#' @param database: STATA file as provided by IFPRI
#' @param path: path of the IFPRI database images to process
#' @param plot: TRUE / FALSE (output summary plots?)
#' @keywords gcc calculation, QA/GC
#' @export
#' @examples
#' # no examples yet

batch.process.database = function(database = NULL,
                                  path = NULL,
                                  plot = FALSE){

  # check if parameters are available
  if ( is.null(database) || is.null(path) ){
    stop("Not all required parameters available!")
  }

  # read in database file
  df = readstata13::read.dta13(database)

  # roi@polygons[[1]]@Polygons[[1]]@coords
  # as.vector(bla$roi@polygons[[1]]@Polygons[[1]]@coords)

  # now loop over all the data and fill generated values
  # this can be parallized to speed up processing if needed
  # using apply would be faster as well, but both won't
  # allow for progress feedback
  # nrow(df)

  # create empty data frame objects
  df$gcc = c()
  df$grvi = c()
  df$roi = c()
  df$horizon = c()

  for (i in 1:10){

    # split out some variable for clarity
    userid = df$uniqueuserid[i]
    reportid = df$reportid[i]
    cropsite = df$uniquecropsiteid[i]

    # create image location string
    image_location = sprintf("%s/%s/%s/%s_%s.jpg",path,userid,cropsite,userid,reportid)

    print(image_location)
    print(file.exists(image_location))

    # calculate the gcc etc values
    values = estimate.gcc(image_location,plot = TRUE)

    # stuff things back into the original dataframe
    df$gcc[i] = values$gcc
    df$grvi[i] = values$grvi
    df$horizon[i] = paste(values$horizon,",")
    df$roi[i] = paste(values$roi,",")
  }

  print(df[1:10,])

}

batch.process.database(database = "/data/Dropbox/Research_Projects/IFPRI/data/Pictures Data 22 Feb 2017.dta",
                       path="/data/Dropbox/Research_Projects/IFPRI/data")

