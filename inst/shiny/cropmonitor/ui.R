# load libraries
require(shiny, quietly = TRUE)
require(shinydashboard, quietly = TRUE)
require(leaflet, quietly = TRUE)
require(plotly ,quietly = TRUE)
require(DT, quietly = TRUE)

# source about page content
about = source('about.r')
help = source('help.r')

# vegetation type list
vegtype = c(
  "All" = "ALL",
  "Water" = "WAT",
  "Evergreen Needleleaf forest" = "ENF",
  "Evergreen Broadleaf forest" = "EBF",
  "Deciduous Needleleaf forest" = "DNF",
  "Deciduous Broadleaf forest" = "DBF",
  "Mixed forest" = "MF",
  "Closed shrublands" = "CSH",
  "Open shrublands" = "OSH",
  "Woody savannas" = "WSA",
  "Savannas" = "BSV",
  "Grasslands" = "GRA",
  "Permanent wetlands" = "PWE",
  "Croplands" = "CRO",
  "Urban and built-up" = "URB",
  "Cropland/Natural vegetation mosaic" = "MOS"
)

# interface elements

header <- dashboardHeader(title = "Ameriflux Explorer")
sidebar <- dashboardSidebar(
  includeCSS("custom.css"),
  sidebarMenu(
    menuItem("Explore data", tabName = "explorer", icon = icon("bar-chart-o")),
    menuItem("About Ameriflux", tabName = "about", icon = icon("info-circle")),
    menuItem("About the package", tabName = "help", icon = icon("info-circle")),
    menuItem("code on GitHub", icon = icon("github"), href = "https://github.com/khufkens/amerifluxr"),
    sidebarUserPanel(name = "Koen Hufkens",
                     image = "https://avatars2.githubusercontent.com/u/1354258?v=3&s=460",
                     subtitle = a("Personal Website", href = "http://www.khufkens.com")
                     )
  )
)

body <- dashboardBody(
  tags$head(
    tags$script(
      HTML("
          window.onload = function() {
            resizeMap();
            resizeTable();
          }
          window.onresize = function() {
            resizeMap();
            resizeTable();
          }
          Shiny.addCustomMessageHandler ('triggerResize',function (val) {
            window.dispatchEvent(new Event('resize'));
          });
          function resizeMap(){
            var h = window.innerHeight - $('.navbar').height() - 280; // Get dashboardBody height
            $('#map').height(h); 
          }
          function resizeTable(){
            var h = window.innerHeight - $('.navbar').height() - 500;
            $('#time_series_plot').height(h);
          }"
      )
    )
  ),
  tags$head(includeCSS("styles.css")),
  tabItems(
    tabItem(
      # the Interactive map and subset interface
      # and time series plotting interface
      tabName = "explorer",
      tabBox(
        side = "left",
        width=12,
        selected = "Map & Site selection",
        tabPanel("Map & Site selection", icon = icon("globe"),
                 fluidRow(
                   valueBoxOutput("site_count"),
                   valueBoxOutput("year_count"),
                   column(4,
                          selectInput("colors", "Vegetation Type",vegtype)
                   )
                 ),
                 fluidRow(
                   column(12,
                          box(width=NULL,
                              leafletOutput("map"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 300, left = "auto", right = 70, bottom = "auto",
                                            width = 320, height = 350,
                                            h4("Climatology of selected sites", align = "center"),
                                            plotOutput("test", height=280,width=280)
                                            )
                          )
                    )
                )
        ),
        tabPanel("Plot data", icon = icon("bar-chart-o"),
                 fluidRow(
                   column(4,
                          box(width = NULL,
                              fluidRow(column(6,
                                              checkboxInput("gap_fill","Gap Filled", value = FALSE, width = NULL)),
                                       column(6,
                                              checkboxInput("refresh","Refresh", value = FALSE, width = NULL))
                                       ),
                              selectInput("productivity", "Ecosystem Productivity",c("NEE (gC m-2 d-1)"="NEE","GPP (gC m-2 d-1)"="GPP"),width="100%"),
                              selectInput("covariate", "Covariate",c("temperature (C)" = "temperature",
                                                                     "precipitation (mm)" = "precipitation",
                                                                     "VPD (kPa)" = "VPD",
                                                                     "PAR (umol m-2 d-1)" = "PAR",
                                                                     "RH (%)" = "RH"),width="100%"),
                              selectInput("plot_type", "Plot Type",c("Time Series"="daily","Yearly Summary"="yearly","NEE phenology"="nee_phen"),width="100%"))),
                   column(8,
                          box(width = NULL,
                              DT::dataTableOutput("table")
                          ))
                 ),
                 fluidRow(
                   column(12,
                          box(width = NULL,
                              plotlyOutput("time_series_plot")
                          )
                   ) 
                 )
        )
      )
    ),
    tabItem(
      # the about page
      tabName = "about",
      tabPanel("About", box(width=NULL,about$value))
    ),
    tabItem(
      # the about page
      tabName = "help",
      tabPanel("Help", box(width=NULL,help$value))
    )
  )
)

ui <- dashboardPage(skin = "blue", header, sidebar, body)