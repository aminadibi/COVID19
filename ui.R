library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
    title = "COVID-19 Dashboard"
)

sidebar <-    dashboardSidebar(
    sidebarMenu(
        menuItem("Latest Maps", tabName = "dashboard"),
        menuItem("Bars", tabName = "bars"),
        menuItem("Trends", tabName = "trends")
#        menuItem("Raw data", tabName = "rawdata")
    )
)

body <- dashboardBody(
    tabItems(
        # First tab content
  tabItem(tabName = "dashboard",
    fluidRow(
        column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   title = "Number of Reported Cases",
                   leafletOutput("caseMap", height = 500),
               ),
               box(width = NULL, solidHeader = TRUE,
                   title = "Rate of Reported Cases",
                   leafletOutput("rateMap", height = 500),
               ),
               box(width = NULL, solidHeader = TRUE,
                   title = "Acceleration of Reported Cases",
                   leafletOutput("accelerationMap", height = 500),
               )
        ),
        column(width = 3,
               
               box(width = NULL, status = "warning",
                   #uiOutput("timeSinceLastUpdate"),
                   actionButton("refresh", "Refresh now"),
                   p(class = "text-muted",
                     br(),
                     paste0("Last updated ", lubridate::now(), " PST"),
                     br(),
                     "Source data updates everyday."
                   )
               )
        )
    )),
    
    tabItem(tabName = "bars",
            fluidRow(
                box(width = NULL, solidHeader = TRUE,
                    plotOutput("barPlotRate", height = 500),
                    plotOutput("barPlotAcceleration", height = 500)),
            )),
  
  tabItem(tabName = "trends",
          fluidRow(
              box(width = NULL, solidHeader = TRUE,
                  plotOutput("trendPlotRate", height = 500),
                  plotOutput("trendPlotAcceleration", height = 500)),
          ))
  ))


dashboardPage(
    header,
    #dashboardSidebar(disable = TRUE),
    sidebar,
    body
)