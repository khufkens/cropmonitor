# load required libraries
require(shiny) # GUI components
require(shinydashboard) # GUI components
require(leaflet) # mapping utility
require(plotly) # fancy ploty in shiny
require(DT) # interactive tables for shiny
require(data.table) # loads data far faster than read.table()

# grab the OS info
user = Sys.info()[6]
machine = Sys.info()[4]

# When on the machine of the developer, sideload the code locally
# for quick reviewing of changes to the GUI
if (machine == "squeeze" | user == "khufkens") {

  # load all functions
  files = list.files(
    "/data/Dropbox/Research_Projects/IFPRI/code/cropmonitor/R",
    "*.r",
    full.names = TRUE
  )
  for (i in files) {
    source(i)
  }
  path = "/data/Dropbox/Research_Projects/IFPRI/code/cropmonitor/inst/shiny/cropmonitor/"
} else {
  path = sprintf("%s/shiny/cropmonitor", path.package("cropmonitor"))
}

# finally read in the metadata if all checks are go
# convert to data frame instead of data table for subsetting
# will change to data table later == faster
df = jsonlite::fromJSON("~/cropmonitor/cropmonitor.json")
df = df[which(df$longitude > 70),]

# read questionairs
if (file.exists("~/cropmonitor/questionaire.xlsx")){
  
  quest = readxl::read_excel("~/cropmonitor/questionaire.xlsx")
  quest = quest[,-(2:9)]
  
  # merge the data on the report id
  df = merge(df, quest, by = 'reportid', all.x = TRUE)
}

# generate strings for thumbs
thumbs = sprintf("~/cropmonitor/thumbs/%s/%s/%s-%s-%s.jpg",
                 df$uniqueuserid,
                 df$uniquecropsiteid,
                 df$uniqueuserid,
                 df$uniquecropsiteid,
                 df$pic_timestamp)

# summarize variables (ugly)
latitude = as.vector(by(df$latitude, INDICES = df$uniquecropsiteid, mean))
longitude = as.vector(by(df$longitude, INDICES = df$uniquecropsiteid, mean))
field = as.vector(by(df$uniquecropsiteid, INDICES = df$uniquecropsiteid, function(x) as.character(x[1]) ))
user = as.vector(by(df$uniqueuserid, INDICES = df$uniquecropsiteid, mean))
image_id = as.vector(by(thumbs, INDICES = df$uniquecropsiteid, function(x) as.character(x[1]) ))
id = 1:length(image_id)

# count the images
image_count = table(df$uniquecropsiteid)
image_count = data.frame(as.character(names(image_count)),as.numeric(image_count))
colnames(image_count) = c('field','images')

# group map data
map_data = data.frame(user, field, latitude, longitude, id)
map_data = merge(map_data, image_count, by = 'field', all.x = TRUE)

# start server routine
server = function(input, output, session) {
  
  # Reactive expression for the data subsetted
  # to what the user selected
  map_click = reactiveValues()

  getValueData = function(table) {
    
    nr_farmers = length(unique(table$uniqueuserid))
    nr_fields = length(unique(table$uniquecropsiteid))
    
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
    return(map_data[,-5])
  },
  selection = "single",
  options = list(lengthMenu = list(c(5, 10), c('5', '10'))),
  extensions = c('Responsive'))
  
  # Render the map of all the sites
  output$map = renderLeaflet({
    map = leaflet(map_data) %>%
      addTiles(
        "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
        attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
        group = "World Imagery"
      ) %>%
      addCircleMarkers(lat = ~latitude,
                       lng = ~longitude,
                       layerId=~id,
                       color = "yellow",
                       radius = 6,
                       stroke = FALSE,
                       fillOpacity = 0.5) %>%
      setView(lng = mean(map_data$longitude),
              lat = mean(map_data$latitude),
              zoom = 8)
  })
  
  # store the click
  #observeEvent(input$map_marker_click,{
  #  image_id = 
  #})
  
  # udpate the data
  processData = function(myrow) {
    
    # if nothing is selected return NULL
    if (length(myrow) == 0) {
      return(NULL)
    }

    # grab the necessary parameters to download the site data
    # subset by field
    field = as.character(map_data$field[as.numeric(myrow)])
    plot_data = df[which(df$uniquecropsiteid == field),]

    if (nrow(plot_data) < 20){
      
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
  
  # preview in the time-series plotting area
  output$preview = renderPlot({
    if (length(input$table_row_last_clicked) != 0) {
      r = raster::brick(image_id[input$table_row_last_clicked])
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
      
      #date = plot_data$datetime
      if (input$plot_type == "gcc") {
        greenness = plot_data$gcc[order(date)]
      } else {
        greenness = plot_data$grvi[order(date)]
      }
      
      # sort stuff
      date = date[order(date)]
      
      # smooth the data using a loess fit
      fit = loess(greenness ~ as.numeric(date), span = 0.3)
      fit_greenness = predict(fit, as.numeric(date), se = TRUE)
      greenness_ci = fit_greenness$se * 1.96
      greenness_smooth = fit_greenness$fit
      
      # if the questionaire data is there plot additional info
      if (any(grepl('q7',names(plot_data)))){
        
        # damage dates
        dam = which(!is.na(plot_data$q7) & grep("50",plot_data$q6))
        
        # Irrigation dates
        irr = grep("Irrigation",plot_data$q10)
        
        print(plot_data$q10)
        
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
            name = "Reported Damage"
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