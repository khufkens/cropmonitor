# server settings

# finally read in the metadata if all checks are go
# convert to data frame instead of data table for subsetting
# will change to data table later == faster

df = readRDS("~/cropmonitor/cropmonitor.rds")
df = df[which(df$longitude > 70),]

# read questionair data
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

# summarize variables (ugly)
latitude = as.vector(by(df$latitude,
                        INDICES = df$userfield,
                        function(x)x[1]))
longitude = as.vector(by(df$longitude,
                         INDICES = df$userfield,
                         function(x)x[1]))

field = as.vector(by(df$userfield,
                     INDICES = df$userfield,
                     function(x) as.character(x[1])))
user = as.vector(by(df$uniqueuserid, INDICES = df$userfield, mean))
image_id = as.vector(by(df$thumbs,
                        INDICES = df$userfield,
                        function(x){as.character(x[1])}))
id = 1:length(image_id)

# count the images
image_count = table(df$userfield)
image_count = data.frame(as.character(names(image_count)),as.numeric(image_count))
colnames(image_count) = c('field','images')

# group map data
map_data = data.frame(field, latitude, longitude)
map_data = merge(map_data, image_count, by = 'field', all.x = TRUE)

# start server routine
server = function(input, output, session) {
  
  # Reactive expression for the data subsetted
  # to what the user selected
  map_click = reactiveValues()

  getValueData = function(table) {
    
    nr_farmers = length(unique(table$old_uniqueuserid))
    nr_fields = length(unique(table$old_uniquecropsiteid))
    
    output$nr_farmers = renderInfoBox({
      valueBox(nr_farmers,
               "Farmers",
               icon = icon("list"),
               color = "blue")
    })
    
    output$nr_fields = renderInfoBox({
      valueBox(nr_fields,
               "Fields",
               icon = icon("list"),
               color = "blue")
    })
  }
  
  # fill site count etc fields
  getValueData(df)
  
  # update the data table used to link to the fields
  output$table = DT::renderDataTable({
    return(map_data)
  },
  selection = "single",
  options = list(lengthMenu = list(c(5, 10), c('5', '10'))),
  extensions = c('Responsive'))
  
  # Render the map of all the sites
  output$map = renderLeaflet({
    map = leaflet(map_data) %>%
      addTiles(
        "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
        attribution = '',
        group = "World Imagery"
      ) %>%
      addMarkers(lat = ~latitude,
                       lng = ~longitude,
                       layerId= ~id,
                       clusterOptions = markerClusterOptions()
                 ) %>%
      setView(lng = mean(map_data$longitude),
              lat = mean(map_data$latitude),
              zoom = 8)
  })
  
  # update the data to be plotted
  processData = function(myrow) {
    
    # if nothing is selected return NULL
    if (length(myrow) == 0) {
      return(NULL)
    }

    # grab the necessary parameters to download the site data
    # subset by field
    field = as.character(map_data$field[as.numeric(myrow)])
    plot_data = df[which(df$userfield == field),]
    
    if (nrow(plot_data) < 10){
      
      # return null if not enough values found
      return(NULL)
      
    } else {
    
      # return this structure
      return(plot_data)
      
    }
  }

  # observe the state of the table, if changed update the data
  inputData = reactive({
    processData(as.numeric(input$table_row_last_clicked))
  })

  # preview in the map area
  output$map_preview = renderPlot({
    if (length(input$map_marker_click$id) != 0) {
      r = raster::brick(image_id[input$map_marker_click$id])
      raster::plotRGB(r)
    } else {
      plot(0,0,
           xlab = '',
           ylab = '',
           xaxt = 'n',
           yaxt = 'n',
           bty = 'n',
           type = 'n')
      text(0,0,labels = "No Preview")
    }
  })
  
  # plot preview from selection
  output$preview <- renderPlot({
    s <- event_data("plotly_click", source = "time_series_plot")
    
    # load data
    plot_data = inputData()
    
    # pick a first preview at the beginning
    # of the series
    loc = ifelse(is.null(s),1,s$pointNumber)
    
    # grab url
    url = plot_data$thumbs[loc]
    url = ifelse(length(url),url,"empty")
    
    if ( file.exists(url) ) {
      r = raster::brick(url)
      raster::plotRGB(r)
    } else {
      plot(0,0,
           xlab = '',
           ylab = '',
           xaxt = 'n',
           yaxt = 'n',
           bty = 'n',
           type = 'n')
      text(0,0,labels = "No Preview")
    }
  })
  
  # grab selection
  output$selection <- renderPrint({
    s <- event_data("plotly_click", source = "time_series_plot")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  # plot the data / MESSY CLEAN UP!!!
  output$time_series_plot = renderPlotly({
    
    # set colours
    image_col = "rgba(102,166,30,0.8)"
    envelope_col = "rgba(128,128,128,0.05)"
    fit_col = "rgba(128,128,128,0.8)"
    
    # load data
    plot_data = inputData()

    if (is.null(plot_data)) {
      
      # format x-axis
      ax <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )

      # Error message depending on the state of the table
      # if selected and no data, or no selection
      if (length(input$table_row_last_clicked) != 0) {
        p = plot_ly(
          x = 0,
          y = 0,
          text = "NO DATA AVAILABLE: CONTACT SITE PI, OR SELECT A NEW SITE FOR PLOTTING",
          mode = "text"
        ) %>%
          layout(xaxis = ax, yaxis = ax)
      } else{
        p = plot_ly(
          x = 0,
          y = 0,
          text = "SELECT A SITE FOR PLOTTING",
          mode = "text"
        ) %>%
          layout(xaxis = ax, yaxis = ax)
      }

    } else{
      
      # sort things, database isn't ordered
      date = as.POSIXct(plot_data$datetime)
      
      if (input$plot_type == "gcc") {
        greenness = plot_data$gcc[order(date)]
      } else if (input$plot_type == "grvi") {
        greenness = plot_data$grvi[order(date)]
      } else if (input$plot_type == "sobel") {
        greenness = plot_data$sobel[order(date)]
      } else if (input$plot_type == "entropy") {
        greenness = plot_data$glcm_entropy[order(date)]
      } else if (input$plot_type == "homogeneity") {
        greenness = plot_data$glcm_homogeneity[order(date)]
      }

      # smooth the data using a loess fit
      fit = loess(greenness ~ as.numeric(date), span = 0.3)
      fit_greenness = predict(fit, as.numeric(date), se = TRUE)
      greenness_ci = fit_greenness$se * 1.96
      greenness_smooth = fit_greenness$fit
      
      # if the questionaire data is there plot additional info
      if (any(grepl('q7',names(plot_data)))){
        
        # damage as in lodging
        dam = which(plot_data$Lodg_winds == 1)
        
        # Irrigation dates
        irr = grep("Irrigation",plot_data$q10)
        
        # Weeding dates
        we = grep("Weeding",plot_data$q10)
        
      }
      
      # check the plotting type
        p = plot_ly(
          x = date,
          y = greenness_smooth,
          mode = "lines",
          type = 'scatter',
          line = list(width = 0),
          source = "time_series_plot",
          showlegend = FALSE
        ) %>%
          add_trace(
            x = date,
            y = greenness_smooth - greenness_ci,
            mode = "lines",
            type = "scatter",
            line = list(width = 0, color = "rgb(200,200,200)"),
            showlegend = FALSE,
            name = "95% CI"
          ) %>%
          add_trace(
            y = greenness_smooth + greenness_ci,
            fill = "tonexty",
            mode = "lines",
            line = list(width = 0, color = "rgb(200,200,200)"),
            showlegend = TRUE,
            name = "95% CI"
          ) %>%
          add_trace(
            y = greenness_smooth,
            mode = "lines",
            line = list(width = 2, color = "rgb(120,120,120)"),
            name = "loess fit",
            showlegend = TRUE
          ) %>%
          add_trace(
            y = greenness,
            mode = "markers",
            type = "scatter",
            marker = list(color = "black", symbol = "circle", size = 5),
            name = input$plot_type,
            showlegend = TRUE
          ) %>%
          add_trace(
            x = date[irr],
            y = greenness_smooth[irr],
            mode = "markers",
            marker = list(color = "#3182bd", symbol = "circle", size = 10),
            showlegend = TRUE,
            name = "Irrigation"
          ) %>%
          add_trace(
            x = date[we],
            y = greenness_smooth[we] + 0.01,
            mode = "markers",
            marker = list(color = "#a1d99b", symbol = "circle", size = 10),
            showlegend = TRUE,
            name = "Weeding"
          ) %>%
          add_trace(
            x = date[dam],
            y = greenness_smooth[dam] - 0.01,
            mode = "markers",
            marker = list(color = "#f03b20", symbol = "circle", size = 10),
            showlegend = TRUE,
            name = "Lodging"
          ) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = input$plot_type),
            showlegend = TRUE,
            title = map_data$field[as.numeric(input$table_row_last_clicked)]
          )
    }
  }) # end plot function
} # server function end