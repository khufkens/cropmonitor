# source about page content
help = source('help.r')

# interface elements
header = dashboardHeader(title = "IFPRI Crop Monitor")
sidebar = dashboardSidebar(
  includeCSS("custom.css"),
  sidebarMenu(
    menuItem("Explore Data", tabName = "explorer", icon = icon("bar-chart-o")),
    menuItem("Help", tabName = "help", icon = icon("info-circle"))
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
            var h = window.innerHeight - $('.navbar').height() - 150; // Get dashboardBody height
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
                   column(9,
                          box(
                              leafletOutput("map"),
                              width = 12
                          )
                    ),
                   column(3,
                          valueBoxOutput("nr_farmers", width = NULL),
                          valueBoxOutput("nr_fields", width = NULL),
                          plotOutput("map_preview")
                          )
                )
        ),
        tabPanel("Plot Data", icon = icon("bar-chart-o"),
                 fluidRow(
                   column(4,
                          box(width = NULL,
                              selectInput("plot_type", "Plot Type",
                                          c("Gcc"="gcc",
                                            "Rcc"="rcc",
                                            "GRVI"="grvi",
                                            "sobel"="sobel",
                                            "entropy"="entropy",
                                            "homogeneity"="homogeneity"
                                            ),
                                          width="100%"),
                              downloadButton("report",
                                             "Generate farmer report"))),
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
      tabName = "help",
      tabPanel("Help", box(width=NULL,help$value))
    )
  )
)

ui = dashboardPage(skin = "green", header, sidebar, body)