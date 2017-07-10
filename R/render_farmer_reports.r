#' Plot farmer reports with damages (non GUI version)
#'
#' @param database: json clean database file (cropmonitor.rds)
#' @param payout: path to payout dta file 
#' @param out_dir: path where to store the output pdfs
#' @keywords reporting
#' @export

render_farmer_reports = function( database = "~/cropmonitor/cropmonitor.rds",
                       payout = "~/cropmonitor/payouts_rabi1617.dta",
                       out_dir = "~"){
  
  # sanity checks, are files there
  if (!file.exists(database)){
    stop('no valid database file selected')
  }
  
  if (!file.exists(payout)){
    stop('no valid database file selected')
  }
  
  # read in all data
  df = readRDS(database)
  df = df[which(df$longitude > 70),]
  
  # create unique field vector
  df$userfield = paste(df$uniqueuserid,df$uniquecropsiteid,sep = "-")
  
  # generate strings for thumbs
  df$thumbs = sprintf("~/cropmonitor/thumbs/%s/%s/%s-%s-%s.jpg",
                      df$old_uniqueuserid,
                      df$old_uniquecropsiteid,
                      df$old_uniqueuserid,
                      df$old_uniquecropsiteid,
                      df$pic_timestamp)
  
  payout = readstata13::read.dta13(payout)
  payout$max_dam_site[is.na(payout$max_dam_site)] = 1
  payout$userfield = paste(payout$HHID,payout$max_dam_site,sep = "-")
  
  # only plot the PBI plots
  loc = grep("PBI",payout$Treatment)
  payout = payout[loc,]
  
  # list all farmers in the payout report
  # loop over all farmers and first field
  farmers = unique(payout$HHID)
  
  for (i in 1:nrow(payout)){

    # pdf filename based on farmer id
    file = sprintf("%s/farmer_%s.pdf",out_dir, payout$HHID[i])

    # subset the large dataframe
    sub = subset(df, userfield == payout$userfield[i])
    
    # maximum damage site
    stats = c(payout$max_dam_numpics[i],
              payout$max_dam[i],
              payout$PBI_payout_hyp[i],
              payout$cat_loss[i],
              payout$reason_pbi[i])

    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport = file.path(tempdir(), "ReportPayout.Rmd")
    template = file.path(path.package("cropmonitor"),"extdata","ReportPayout.Rmd")

    # copy template
    file.copy(template,
              tempReport,
              overwrite = TRUE)

    # grab data
    params = list(n = sub,
                  stats = stats)

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport,
                      output_file = file,
                      params = params,
                     envir = new.env(parent = globalenv()))
    if (i == 10){
      break
    }
  }
}

#render_farmer_reports()
