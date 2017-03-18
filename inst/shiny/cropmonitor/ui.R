# load libraries
require(shiny, quietly = TRUE)
require(shinydashboard, quietly = TRUE)
require(leaflet, quietly = TRUE)
require(plotly ,quietly = TRUE)
require(DT, quietly = TRUE)

# source about page content
about = source('about.r')
help = source('help.r')

# interface elements

header = dashboardHeader(title = "IFPRI Crop Monitor")
sidebar = dashboardSidebar(
  includeCSS("custom.css"),
  sidebarMenu(
    menuItem("Explore data", tabName = "explorer", icon = icon("bar-chart-o")),
    menuItem("Help", tabName = "help", icon = icon("info-circle")),
    menuItem("code on GitHub", icon = icon("github"), href = "https://github.com/khufkens/cropmonitor")
  )
)

body = dashboardBody(
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
            var h = window.innerHeight - $('.navbar').height() - 490;
            $('#time_series_plot').height(h);
            $('#preview').height(h);
          }
          "
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
        selected = "Map",
        tabPanel("Map", icon = icon("globe"),
                 fluidRow(
                   column(4,
                          valueBoxOutput("nr_farmers", width = NULL)
                   ),
                   column(4,
                          valueBoxOutput("nr_fields", width = NULL)
                   ),
                   column(4,
                          selectInput('in1', 'Farmer', c(Choose='', state.name), selectize=FALSE)
                   )
                 ),
                 fluidRow(
                   column(12,
                          box(
                              leafletOutput("map"),
                              width = 12
                          )
                    )
                )
        ),
        tabPanel("Plot data", icon = icon("bar-chart-o"),
                 fluidRow(
                   column(4,
                          box(width = NULL,
                              selectInput("plot_type", "Plot Type",
                                          c("Gcc"="gcc","GRVI"="grvi"),
                                          width="100%"))),
                   column(8,
                          box(width = NULL,
                              DT::dataTableOutput("table")
                          ))
                 ),
                 fluidRow(
                   column(4,
                     box(
                       width = NULL,
                       plotOutput("preview")
                     )
                   ),
                   column(8,
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

ui = dashboardPage(skin = "green", header, sidebar, body)