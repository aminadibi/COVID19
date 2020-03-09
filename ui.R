library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
    title = "COVID-19 Dashboard"
)

sidebar <-    dashboardSidebar(
    sidebarMenu(
        menuItem("Latest Maps", tabName = "dashboard"),
        menuItem("Trends", tabName = "trends"),
        menuItem("Raw data", tabName = "rawdata")
    )
)

body <- dashboardBody(
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
    )
)

dashboardPage(
    header,
    #dashboardSidebar(disable = TRUE),
    sidebar,
    body
)