# load required libraries
require(shiny) # GUI components
require(shinydashboard) # GUI components
require(leaflet) # mapping utility
require(plotly) # fancy ploty in shiny
require(DT) # interactive tables for shiny
require(data.table) # loads data far faster than read.table()

# grab the OS info
OS = Sys.info()[1]
machine = Sys.info()[4]

# When on the machine of the developer, sideload the code locally
# for quick reviewing of changes to the GUI
if (machine == "squeeze" | machine == "khufkens") {

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
#df = jsonlite::fromJSON("~/cropmonitor/cropmonitor.json")
df = readstata13::read.dta13("/data/Dropbox/Research_Projects/IFPRI/data/Pictures Data CLEAN 03_03_17.dta")
df = df[which(df$longitude > 70),]

latitude = as.vector(by(df$latitude, INDICES = df$uniquecropsiteid, mean))
longitude = as.vector(by(df$longitude, INDICES = df$uniquecropsiteid, mean))
field = as.vector(by(df$uniquecropsiteid, INDICES = df$uniquecropsiteid, function(x) as.character(x[1]) ))
image_id = as.vector(by(thumbs, INDICES = df$uniquecropsiteid, function(x) as.character(x[1]) ))
user = as.vector(by(df$uniqueuserid, INDICES = df$uniquecropsiteid, mean))

map_data = data.frame(latitude,longitude,field,user,image_id)
map_data$preview = apply(map_data, 1, function(x)
  paste(
    "<table width=400px, border=0px>",
    
    "<tr>",
    "<td>",
    "User: ",
    x[4],
    "</td>",
    "</tr>",
    
    "<tr>",
    "<td>",
    "Field: ",
    x[3],
    "</td>",
    "</tr>",
    
    "</table>",
    sep = ""
  ))

# start server routine
server = function(input, output, session) {
  
  # Reactive expression for the data subsetted
  # to what the user selected
  v1 = reactiveValues()
  v2 = reactiveValues()
  reset = reactiveValues()
  row_clicked = reactiveValues()

  getValueData = function(table) {
    nr_sites = length(unique(map_data$user))
    output$site_count = renderInfoBox({
      valueBox(nr_sites,
               "Sites",
               icon = icon("list"),
               color = "blue")
    })
  }

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
                       popup= ~preview,
                       color = "yellow",
                       radius = 6,
                       stroke = FALSE,
                       fillOpacity = 0.5) %>%
      setView(lng = 73,
              lat = 30.5,
              zoom = 8)
  })

  downloadData = function(myrow) {
    
    # if nothing is selected return NULL
    if (length(myrow) == 0) {
      return(NULL)
    }

    # if there is a site selected but it does not have data online
    # return NULL as well
    if (df$online_data[as.numeric(myrow)] == "no") {
      return(NULL)
    }

    # convert binary gap filling values to text
    if (gaps == FALSE) {
      gaps_label = "WG"
    } else{
      gaps_label = "GF"
    }

    # grab the necessary parameters to download the site data
    site = df$site_id[as.numeric(myrow)]

    # read the data
    data = read.ameriflux(file)

    # Aggregating to daily values
    progress$set(value = 0.4, detail = "aggregating to daily values")
    plot_data = aggregate.flux(data)

    # combine into one data frame
    plot_data = data.frame(plot_data, smooth_gpp, smooth_nee)

    # nee transition dates
    if (any(!is.na(plot_data$NEE_smooth))) {
      transitions = nee.transitions(plot_data)
    } else{
      transitions = NULL
    }

    # combine data in nested list
    output = list(plot_data, transitions)

    # return this structure
    return(output)
  }

  # observe the state of the table, if changed update the data
  inputData = reactive({
    downloadData(
      as.numeric(input$table_row_last_clicked))
  })

  # plot the data / MESSY CLEAN UP!!!
  output$time_series_plot = renderPlotly({
    
    # set colours
    labels_covariate_col = "rgb(231,41,138)"
    covariate_col = "rgba(231,41,138,0.4)"
    flux_col = "rgba(102,166,30,0.8)"
    envelope_col = "rgba(128,128,128,0.05)"
    ltm_col = "rgba(128,128,128,0.8)"

    # load data

    data = inputData()
    plot_data = data[[1]]
    transition_data = data[[2]]

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
      # subset data according to input / for some reason I can't call the
      # data frame columns using their input$... name
      plot_data$flux = plot_data[, which(colnames(plot_data) == input$productivity)]
      plot_data$flux_smooth = plot_data[, which(colnames(plot_data) == sprintf("%s_smooth", input$productivity))]

      # include cummulative values in plotting, should be easier to interpret
      # the yearly summary plots
      plot_data$covariate = plot_data[, which(colnames(plot_data) == input$covariate)]

      # check the plotting type
      if (input$plot_type == "daily") {

        # format y-axis
        ay1 = list(title = input$productivity,
                   showgrid = FALSE)

        ay2 <- list(
          tickfont = list(color = labels_covariate_col),
          titlefont = list(color = labels_covariate_col),
          overlaying = "y",
          title = input$covariate,
          side = "right",
          showgrid = FALSE
        )

        p = plot_ly(
          data = plot_data,
          x = ~date,
          y = ~flux,
          mode = "lines",
          type = 'scatter',
          name = input$productivity,
          line = list(color = flux_col)
        ) %>%
          add_trace(
            y = ~covariate,
            mode = "lines",
            type = 'scatter',
            yaxis = "y2",
            line = list(color = covariate_col),
            name = input$covariate
          ) %>%
          layout(
            xaxis = list(title = "Date"),
            yaxis = ay1,
            yaxis2 = ay2,
            showlegend = TRUE,
            title = df$site_id[as.numeric(input$table_row_last_clicked)]
          )
      } else if (input$plot_type == "yearly") {

        # long term mean flux data
        ltm = plot_data %>% group_by(doy) %>%
          summarise(mn = mean(flux_smooth), sd = sd(flux_smooth), doymn = mean(doy))

        p = ltm %>% plot_ly(
          x = ~ doymn,
          y = ~ mn,
          mode = "lines",
          type = 'scatter',
          name = "LTM",
          line = list(color = ltm_col),
          inherit = FALSE
        ) %>%
          add_trace(
            x = ~ doymn,
            y = ~ mn - sd,
            mode = "lines",
            type = 'scatter',
            fill = "none",
            line = list(width = 0, color = envelope_col),
            showlegend = FALSE,
            name = "SD"
          ) %>%
          add_trace(
            x = ~ doymn,
            y = ~ mn + sd,
            type = 'scatter',
            mode = "lines",
            fill = "tonexty",
            line = list(width = 0, color = envelope_col),
            showlegend = TRUE,
            name = "SD"
          ) %>%
          add_trace(data = plot_data,
                    x = ~ doy,
                    y = ~ flux_smooth,
                    split = ~ year,
                    type = "scatter",
                    mode = "lines",
                    line = list(color = "Set1"),
                    showlegend = TRUE
          ) %>%
          layout(
            xaxis = list(title = "DOY"),
            yaxis = list(title = input$productivity),
            title = df$site_id[as.numeric(input$table_row_last_clicked)]
          )

      } else if (input$plot_type == "nee_phen") {
        if (is.null(transition_data)) {
          # format x-axis
          ax <- list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE
          )
          p = plot_ly(
            x = 0,
            y = 0,
            text = "NO NEE data available",
            mode = "text"
          ) %>% layout(xaxis = ax, yaxis = ax)
        } else{
          sos_col = "rgb(231,41,138)"
          eos_col = "rgba(231,41,138,0.4)"
          gsl_col = "rgba(102,166,30,0.8)"

          ay1 = list(title = "DOY",
                     showgrid = FALSE)

          ay2 <- list(
            overlaying = "y",
            title = "Days",
            side = "left",
            showgrid = FALSE
          )

          # regression stats
          reg_sos = lm(transition_data$SOS_NEE_smooth ~ transition_data$year)
          reg_eos = lm(transition_data$EOS_NEE_smooth ~ transition_data$year)
          reg_gsl = lm(transition_data$GSL_NEE_smooth ~ transition_data$year)

          # summaries
          reg_gsl_sum = summary(reg_gsl)
          reg_eos_sum = summary(reg_eos)
          reg_sos_sum = summary(reg_sos)

          # r-squared and slope
          r2_gsl =  round(reg_gsl_sum$r.squared, 2)
          slp_gsl = round(reg_gsl_sum$coefficients[2, 1], 2)
          r2_sos = round(reg_sos_sum$r.squared, 2)
          slp_sos = round(reg_sos_sum$coefficients[2, 1], 2)
          r2_eos = round(reg_eos_sum$r.squared, 2)
          slp_eos = round(reg_eos_sum$coefficients[2, 1], 2)

          p1 = plot_ly(
            x = transition_data$year,
            y = transition_data$SOS_NEE_smooth,
            mode = "markers",
            type = "scatter",
            name = "SOS"
          ) %>%
            add_trace(
              x = transition_data$year,
              y = reg_sos$fitted.values,
              mode = "lines",
              type = "scatter",
              name = sprintf("R2: %s| slope: %s", r2_sos, slp_sos),
              line = list(width = 2)
            ) %>%
            add_trace(
              x = transition_data$year,
              y = transition_data$EOS_NEE_smooth,
              mode = "markers",
              type = "scatter",
              name = "EOS"
            ) %>%
            add_trace(
              x = transition_data$year,
              y = reg_eos$fitted.values,
              type = "scatter",
              mode = "lines",
              name = sprintf("R2: %s| slope: %s", r2_eos, slp_eos),
              line = list(width = 2)
            )

          p2 = plot_ly(
            x = transition_data$year,
            y = transition_data$GSL_NEE_smooth,
            mode = "markers",
            type = "scatter",
            name = "GSL"
          ) %>%
            add_trace(
              x = transition_data$year,
              y = reg_gsl$fitted.values,
              type = "scatter",
              mode = "lines",
              name = sprintf("R2: %s| slope: %s", r2_gsl, slp_gsl),
              line = list(width = 2)
            )

          p = subplot(p1, p2, margin = 0.05) %>%
            layout(
              xaxis = list(title = "Year"),
              yaxis = ay1,
              xaxis2 = list(title = "Year"),
              title = df$site_id[as.numeric(input$table_row_last_clicked)],
              yaxis2 = ay2,
              showlegend = TRUE
            )
        }
      }
    }
  }) # end plot function
} # server function end