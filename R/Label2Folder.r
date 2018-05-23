#'
#' Takes labels and moves associated files to folders for tensor training
#'
#'
#' @param database: path and name of RDS or dta file as provided by IFPRI with images where labels will be stored
#' @param database_label: column of database where labels are stored
#' @param database_image_col_name: column of database where names, urls or paths to images are stored
#' @param image_path: path of folder holding aoi images
#' @param image_ext: extension type of image for example .jpg
#' @param out_image_path: path of folder where images should be sorted for training
#' @keywords
#' @export
#' @examples
#'
#' \dontrun{
#' image_path = '/media/ssd/Lodging_Classifier/aoi/'
#' image_ext = '.jpg'
#' database = '/media/ssd/Lodging_Classifier/cropmonitor_merged.rds'
#' database_label = 'Lodging'
#' database_image_col_name = 'image'
#' out_image_path = '/media/ssd/Lodging_Classifier/sorted'
#' Label2Folder(database,image_path,image_ext='.jpg',out_image_path)
#' }

Label2Folder = function(database,image_path,image_ext='.jpg',out_image_path){

    # read in database
    database_ext = strsplit(database,".", fixed = TRUE)[[1]][2]
    if(database_ext =='rds' | database_ext == 'RDS') in_data = readRDS(database) else
      if(database_ext =='dta' ){require(readstata13); in_data = read.dta13(database)} else
        print('database type unknown please provide dta or rds file')

    # find images to be sorted
    files_to_process = list.files(image_path,pattern = image_ext,recursive = T)

    # create a progress bar
    pb = txtProgressBar(min = 0, max = length(files_to_process), style = 3)
    k = 0

    # create output folder
    destination = file.path(out_image_path, database_label)
    print(paste('Files will be moved to:',destination))
    dir.create( destination, showWarnings = F)

    # get files names in database
    database_image = basename(in_data[,database_image_col_name])

    for(file_to_process in files_to_process){

        # set progress bar
        setTxtProgressBar(pb, k)
        k = k + 1

        # grab basename of origin file
        file_name_to_process = basename(file_to_process)

        # if file is listed in the database copy to appropriate folder
        if(file_name_to_process %in% database_image){
          # get label for image from database
          label = in_data[which(database_image == file_name_to_process),database_label]
          # create directory if needed
          dir.create( file.path(out_image_path,database_label,label), showWarnings = F)
          # copy file
          try(file.copy(from = file.path(image_path,file_to_process),
                    to = file.path(out_image_path,database_label,label,file_name_to_process),overwrite = T))

        }

    }
}
