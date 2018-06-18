#' Estimates the area under the curve (auc)
#' for a smoothed PBI time series. Data are
#' normalized to allow for comparisons of rate
#' of change.
#' 
#' @param df: cropmonitor data frame / database file
#' @param span: smoothing factor for loess fitting
#' @keywords time series, index
#' @export
#' @examples
#' # no examples yet

auc = function(df, span = 0.3){
  
  # check length
  if (nrow(df) <= 10){
    return(list("auc" = NA,
                "auc_left" = NA,
                "auc_right" = NA))
  }
  
  # standard normalize between 0 - 1
  normalize = function(x){
    (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
  }
  
  # sort things, database isn't ordered  
  gcc = df$gcc
  date = df$date
  
  # create a complete time series (all dates, non sparse)
  full_date_range = seq(min(df$date),max(df$date),by = "days")  
  
  # smooth the data using a loess fit
  fit = try(loess(gcc ~ as.numeric(date), span = 0.3))
  if (inherits(fit, "try-error")){
    return(list("auc" = NA,
                "auc_left" = NA,
                "auc_right" = NA))
  }
  
  fit_gcc = try(predict(fit, as.numeric(full_date_range), se = FALSE))
  if (inherits(fit_gcc, "try-error")){
    return(list("auc" = NA,
                "auc_left" = NA,
                "auc_right" = NA))
  }

  # normalize
  fit_gcc = normalize(fit_gcc)
  
  # grab first occurence of value equal to max_gcc
  max_loc = which(fit_gcc == max(fit_gcc, na.rm = TRUE))[1]
  
  # sum everything under the curve
  auc = sum(fit_gcc, na.rm = TRUE)
  
  # grab sum
  auc_left = sum(fit_gcc[1:max_loc], na.rm = TRUE)
  auc_right = sum(fit_gcc[max_loc:length(fit_gcc)], na.rm = TRUE)
  
  # return everything nicely
  return(list("auc" = auc,
              "auc_left" = auc_left,
              "auc_right" = auc_right))
}