#' Calculates a smoothed fit for a set of values, a span value
#' can be set / found by using optimal_span()
#'
#' @param y a vector with measurement values to smooth
#' @param x a vector with dates / time steps
#' @param span optional values to weigh the loess fit with
#' @keywords smoother, span, loess, time series
#' @export

smooth_function = function(x = NULL,
                           y = NULL,
                           span = 0.4){
  
  # fill in x values if missing
  if (is.null(x)){
    x = 1:length(y)
  }
  
  # fit the data and return the fit object as a whole
  # no formatting provided
  fit = loess(y ~ as.numeric(x), span = span)
  return(list(values = predict(fit, as.numeric(x)),
              fit = fit))
}