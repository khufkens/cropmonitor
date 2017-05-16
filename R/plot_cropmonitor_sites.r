#' Plot Gcc and GRVI values for all fields (sites), for quick
#' non interactive review or reporting
#'
#' @param database: json clean database file
#' @param span: smoothing parameter (% of data points)
#' @param out_dir: path where to store the output pdfs
#' @keywords gcc / grvi calculation, QA/GC
#' @export
#' @examples
#' 
#' 

plot_cropmonitor_sites = function( database = "~/cropmonitor/cropmonitor.json",
                       questionaire = "~/cropmonitor/questionaire.xlsx",
                       span = 0.3,
                       out_dir = "~/cropmonitor"){
  
  # plotting function, called later
  plot_site = function(df, out_dir = out_dir, span){
  
    # skip site with less than 20 images
    if (nrow(df) < 20){
      return(NULL)
    }
  
    print(df$q10)
    
    # sort things, database isn't ordered  
    grvi = df$grvi[order(df$date)]
    gcc = df$gcc[order(df$date)]
    date = df$date[order(df$date)]
  
    # smooth the data using a loess fit
    fit_gcc = loess(gcc ~ as.numeric(date), span = span)
    fit_grvi = loess(grvi ~ as.numeric(date), span = span)
    
    pdf(sprintf("%s/output/%s-%s.pdf",out_dir,
                unique(df$uniqueuserid),
                unique(df$uniquecropsiteid)),
        6,8)

    # set plotting parameters
    par(mar=c(5,4,4,5)+.1,
        mfrow = c(2,1),
        tck = 0.03,
        lwd = 2)
    
    # plot the data
    plot(date,gcc,xlab='',ylab='GCC',xaxt='n',pch=16)
    lines(fit_gcc$x,
          fit_gcc$fitted,
          lwd=2,
          ylim = c(0,1))
    
    # labels on axis etc
    tlab = seq(min(date), max(date), length.out=6)
    lab = format(tlab,format="%Y-%m-%d")
    axis(1, at=tlab, labels=FALSE)
    text(x=tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
         labels=lab, srt=35, adj=1, xpd=TRUE)
    
    # initiate a new plot and plot data
    par(new=TRUE)
    plot(date,grvi,xlab='',ylab='',pch=16,col='gray',xaxt="n",yaxt="n")
    lines(fit_grvi$x,
          fit_grvi$fitted,
          lwd=2,col='gray')
    
    # labels and axis
    axis(4)
    mtext("GRVI",side=4,line=3)
    
    header = sprintf("%s (User ID: %s )\n Crop Site: %s",
                     unique(df$cropsite_name),
                     unique(df$uniqueuserid),
                     unique(df$uniquecropsiteid))
    mtext(header,
          side = 3,
          line = -3,
          outer = TRUE,
          font = 2)
    
    # if the questionaire data is there plot additional info
    if (any(grepl('q7',names(df)))){
      
      # plot additional info regarding damage and management
      dam = which(!is.na(df$q7) & grep("50",df$q6))
      abline(v = date[dam-1], col = "pink")
      
      # Irrigation
      irr = grep("Irrigation",df$q10)
      abline(v = date[irr-1], col = "lightblue")
      
      # Weeding
      we = grep("Weeding",df$q10)
      abline(v = date[we-1], col = "green")
      
    }
    
    # new plot
    plot(
      1,
      1,
      xlab = '',
      ylab = '',
      xaxt = 'n',
      yaxt = 'n',
      type = 'n',
      bty = 'n'
    )
    
    # plot legend(s)
    legend(
      "topleft",
      col = c('black', 'gray'),
      bg = "white",
      pch = 16,
      legend = c("GCC", "GRVI"),
      title = 'Vegetation Indices',
      bty = 'n'
    )
    
    legend(
      "topright",
      col = c('lightblue', 'pink', 'green'),
      pch = 16,
      legend = c('Irrigation', 'Damage', 'Weeding'),
      title = 'Self Reported Observations',
      bty = 'n'
    )
    
    # remove variables
    rm('dam','we','irr')
    
    # shut down device
    dev.off()
  }
   
  # read local database
  df = jsonlite::fromJSON(database)
  
  # generate time vector
  df$date = as.Date(df$date)
  
  # read questionair
  if (!is.null(questionaire)){
    quest = readxl::read_excel(questionaire)
    quest = quest[,-(2:9)]
    
    # merge the data on the report id
    df = merge(df, quest, by = 'reportid', all.x = TRUE)
  }
  
  # evaluate by (basically a tidy loop) 
  by(df,INDICES = df$uniquecropsiteid, function(x){
    plot_site(x,
              out_dir = out_dir,
              span = span)
    })
}

# plot_cropmonitor_sites(database = "/data/Dropbox/Research_Projects/IFPRI/data/cropmonitor_bak.json",
#           questionaire = "/data/Dropbox/Research_Projects/IFPRI/data/Questionnaire Answers 03_03_17.xlsx")