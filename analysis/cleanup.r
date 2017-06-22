# clean up that fucking mess of DTA data format
df = jsonlite::read_json("~/cropmonitor/cropmonitor.json")

g1 = unlist(lapply(df, function(x)if(!is.null(x$`1`)){x$`1`}else{NA}))
g2 = unlist(lapply(df, function(x)if(!is.null(x$`2`)){x$`2`}else{NA}))
g3= unlist(lapply(df, function(x)if(!is.null(x$`3`)){x$`3`}else{NA}))
g4 = unlist(lapply(df, function(x)if(!is.null(x$`4`)){x$`4`}else{NA}))
g5 = unlist(lapply(df, function(x)if(!is.null(x$`5`)){x$`5`}else{NA}))
g6 = unlist(lapply(df, function(x)if(!is.null(x$`6`)){x$`6`}else{NA}))
g7 = unlist(lapply(df, function(x)if(!is.null(x$`7`)){x$`7`}else{NA}))
g8 = unlist(lapply(df, function(x)if(!is.null(x$`8`)){x$`8`}else{NA}))
g9 = unlist(lapply(df, function(x)if(!is.null(x$`9`)){x$`9`}else{NA}))

output = cbind(g1,g2,g3,g4,g5,g6,g7,g8,g9)

colnames(output) = c("gcc_90",
                  "grvi",
                  "glcm_variance",
                  "glcm_homogeneity",
                  "glcm_contrast",
                  "glcm_dissimilarity",
                  "glcm_entropy",
                  "glcm_second_moment",
                  "sobel")

dta = readstata13::read.dta13("/data/Dropbox/Research_Projects/IFPRI/data/Pictures Data CLEAN 04_21_17.dta")

combined_data = data.frame(dta,output)
saveRDS(combined_data, file = "~/cropmonitor/cropmonitor.rds")

