
#'
#' Takes label folders for tensor training and stores labels  in the image database
#'
#' Files should be sorted in the following order:
#' out_image_path:
#'       LabelFolder1:
#'            TrueFolder:
#'                Image_i.jpg
#'            FalseFolder:
#'                Image_i.jpg
#'            OtherFolder:
#'      LabelFolder2:
#'            TrueFolder:
#'                Image_i.jpg
#'            FalseFolder:
#'            OtherFolder:
#'
#'
#' @param database: path and name of RDS or dta file as provided by IFPRI with images where labels will be stored
#' @param image_ext: extension type of image for example .jpg
#' @param out_image_path: path of folder where images were sorted for training
#' @keywords label sort
#' @export
#' @examples
#'
#' \dontrun{
#' image_ext = '.jpg'
#' database = '/media/ssd/Lodging_Classifier/cropmonitor_merged.rds'
#' out_image_path = '/media/ssd/Lodging_Classifier/sorted'
#' in_data = Folder2Label(database,image_path,image_ext='.jpg',out_image_path)
#' saveRDS(in_data,'/media/ssd/Lodging_Classifier/cropmonitor_merged_updated_images.rds')
#'
#' table(in_data$Lodging)
#' }

Folder2Label = function(database,image_path,image_ext='.jpg',out_image_path){
    # read in database
    database_ext = strsplit(database,".", fixed = TRUE)[[1]][2]
    if(database_ext =='rds' | database_ext == 'RDS') in_data = readRDS(database) else
      if(database_ext =='dta' ){require(readstata13); in_data = read.dta13(database)} else
        print('database type unknown please provide dta or rds file')

    # find images to be sorted
    files_to_process = list.files(out_image_path,pattern = image_ext,recursive = T, full.names = T)

    # split path into column name for database, label, and file name
    grab_last_dir = function(x) unlist(strsplit(dirname(x),'/'))[length(unlist(strsplit(dirname(x),'/')))]
    grab_2ndlast_dir = function(x) unlist(strsplit(dirname(x),'/'))[length(unlist(strsplit(dirname(x),'/')))-1]

    column = sapply(files_to_process, grab_2ndlast_dir,USE.NAMES = F)
    label = sapply(files_to_process, grab_last_dir,USE.NAMES = F)
    file = basename(files_to_process)

    # create database columns with correct names
    in_data[ unique(column)] = NA

    # for ecah unique column name add appropriate labels
    for(column_name in unique(column)){

        # find file location in database (returns row
        # in files_to_process that matches database image location) use
        # e.g. label[match_order] file[match_order]
        match_order = match( basename(in_data$image),file[column==column_name] )

        # replace values of _sorted column with appropriate label
        in_data[,column_name] = label[match_order]

    }
    return(in_data)
}


