tags$html(
  tags$head(
    tags$title('About page')
  ),
  tags$body(
    tags$h2('The IFPRI Crop Monitor package'),
    tags$p('This R package provides functions to easily process the Picture Based Insurance data as collected during a first field trial. Data was collected through a cellphone based platform. The package provides an easy way to process internally processed STATA files. The package takes this STATA files, downloads the necessary additional image data from a private IFPRI server and pre-processes it into a consistent format. By default downloaded data are stored in "cropmonitor" directory in the home directory of the workstaton processing the data. In there, the directory images holds downsampled image files of the originals, corrected for orientation errors. In addition thumbnails are stored in the thumbs directory. Data is organized using the structure as used in the original database associated with the cellphone application sourcing the data.'),
    tags$p('To generate this initial database of source data use the update_image_db() function associated with this package. Downloading and especially orienting the images correctly might take a significant time (e.g. 10K images will take 2 days to process on a moderately fast system). Once this initial step is executed incremental changes can be made, taking up less time. To update the final database which is associated with this GUI run the process_image_db() function in an R session. This will calculate greenness vegetation indices based upon the downloaded images, as well as texture metrics and edge detection values for a given region of interest (ROI) as automatically defined through a horizon finding algorithm.')
  )
)
