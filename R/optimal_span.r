#' Calculates the optimal span for a loess spline
#' smoother based upon the bayesian information
#' criterion (BIC).
#'
#' @param y: a vector with measurement values to smooth
#' @param x: a vector with dates / time steps
#' @param weights: optional values to weigh the loess fit with
#' @param step: span increment size
#' @keywords smoother, span, loess, time series
#' @export
#' @examples
#'
#' \dontrun{
#' # Internal function only, should not be used stand-alone.
#' 
#' l = sin(1,10,0.01)
#' l = l + runif(length(l)))
#' optimal.span(l, plot = TRUE)
#' }

# This function takes a data frame with smoothed gcc
# values and CI as input, this data is generated by
# the smooth_ts() routine or is already present in the
# downloaded phenocam file if downloaded with the default
# settings for download.phenocam()

optimal_span = function(y,x=NULL,
                        weights=NULL,
                        start = 0.01,
                        step=0.01,
                        label = NULL,
                        plot = FALSE){

  # custom AIC function which accepts loess regressions
  myAIC = function(x){

    if (!(inherits(x,"loess"))){
      stop("Error: argument must be a loess object")
    }

    # extract loess object parameters
    n = x$n
    traceL = x$trace.hat
    sigma2 = sum( x$residuals^2 ) / (n-1)
    delta1 = x$one.delta
    delta2 = x$two.delta
    enp = x$enp

    # calculate AICc1
    # as formulated by Clifford M. Hurvich; Jeffrey S. Simonoff; Chih-Ling Tsai (1998)
    AICc1 = n*log(sigma2) + n* ( (delta1/delta2)*(n+enp)/(delta1^2/delta2)-2 )

    if(is.na(AICc1) | is.infinite(AICc1)){
      return(NA)
    }else{
      return(AICc1)
    }
  }

  # create numerator if there is none
  if (is.null(x)){
    x = 1:length(y)
  }

  # return AIC for a loess function with a given span
  loessAIC = function(span){
    # check if there are weights, if so use them
    if ( is.null(weights) ){
      fit = suppressWarnings(try(loess(y ~ as.numeric(x),
                                       span=span),
                                 silent=TRUE))
    } else {
      fit = suppressWarnings(try(loess(y ~ as.numeric(x),
                                       span=span, 
                                       weights = weights),
                                 silent=TRUE))
    }

    # check if the fit failed if so return NA
    if (inherits(fit,"try-error")){
      return(NA)
    }else{
      return(myAIC(fit))
    }
  }

  # parameter range
  span = seq(start,1,by=step)

  # temporary AIC matrix, lapply loop
  # (instead of for loop) cleaner syntax
  tmp = unlist(lapply(span,loessAIC))
  
  # find the optimal span as the minimal AICc1 value
  # in the calculated range (span variable)
  opt.span = span[which(tmp == min(tmp,na.rm=TRUE))][1]
  
  # plot the optimization if requested
  if (plot == TRUE){
    
    par(mfrow = c(2,1))
    plot(as.numeric(x),y,
         xlab = 'value',
         ylab = 'Gcc',
         type = 'p',
         pch = 19,
         main = label)
    
    col = rainbow(length(span),alpha = 0.5)
    
    for (i in 1:length(span)){
      fit = loess(y ~ as.numeric(x),
                  span = span[i])
      lines(fit$x,
            fit$fitted,
            lwd = 1,
            col = col[i])
    }
    
    fit = loess(y ~ as.numeric(x),
                span = opt.span)
    
    lines(fit$x,
          fit$fitted,
          lwd = 3,
          col = 'black',
          lty = 1)
    
    plot(span,
         tmp,
         pch = 19,
         type = 'p',
         ylab = 'AICc1',
         col = col)
    
    abline(v = opt.span,col = 'black')
    
  }

  # trap error and return optimal span
  if (is.na(opt.span)) {
    return(NULL)
  } else {
    return(opt.span)
  }
}
