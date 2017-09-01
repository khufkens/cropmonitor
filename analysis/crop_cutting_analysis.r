# Load main data file, including all indices
df = readRDS("~/cropmonitor/cropmonitor.rds")

# screen for test sites in the US / EU
df = df[which(df$longitude > 70),]
df = df[, -which(names(df) %in% "questionnaireresult")]
df = df[which(as.Date(df$date) > as.Date("2016-10-01")),]

# read in the crop cutting data (metric version g/m )
crop_data = readstata13::read.dta13("~/cropmonitor/crop_cutting_master_metric.dta")

# read questionnaire data and merges with the original
# data
if (file.exists("~/cropmonitor/questionaire.xlsx")){
  quest = readxl::read_excel("~/cropmonitor/questionaire.xlsx")
  quest = quest[,-(2:9)]
  df = merge(df, quest, by = 'reportid', all.x = TRUE)
}

# generate strings for thumbs
df$thumbs = sprintf("~/cropmonitor/thumbs/%s/%s/%s-%s-%s.jpg",
                    df$old_uniqueuserid,
                    df$old_uniquecropsiteid,
                    df$old_uniqueuserid,
                    df$old_uniquecropsiteid,
                    df$pic_timestamp)

# create unique field vector
df$userfield = paste(df$uniqueuserid,df$uniquecropsiteid,sep = "-")

# grab auc data
field_metrics = lapply(unique(df$userfield),function(x){
  
  sub = df[which(df$userfield == x),]
  auc_results = auc(sub)
  userid = sub$uniqueuserid[1]
  weeks = ceiling(as.numeric(difftime(max(sub$date),
                             min(sub$date),
                             units = c("weeks"))))
  
  return(list("FarmerID" = userid,
              "auc" = auc_results$auc,
              "auc_left" = auc_results$auc_left,
              "auc_right" = auc_results$auc_right,
              "image_count" = nrow(sub),
              "nr_weeks" = weeks))
})

# summarize metrics
field_metrics = data.frame(do.call(rbind.data.frame, field_metrics))
field_metrics$images_week = field_metrics$image_count / field_metrics$nr_weeks

# grab the area under the curve for all sites merge the data
#test = merge(crop_data, field_metrics, by = 'FarmerID', all.x = TRUE)

# summary stats for a gcc time series
#time_series_summary = function(df, metric = "gcc"){
  
  # summary stats
  res = df %>% 
    group_by(date) %>% 
    summarise_(mean = mean(as.name(metric), na.rm = TRUE),
              sd = sd(as.name(metric),na.rm = TRUE))

  # plotting
  ggplot(data = res, aes(x = date, y = mean)) +
    geom_smooth(method = "loess", span = 0.3) +
    geom_point()
#}

#time_series_summary(df, "grvi")

  
  
  
  