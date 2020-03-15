library(shinydashboard)
library(leaflet)
library(shinycssloaders)

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
                   leafletOutput("caseMap", height = 500) %>% withSpinner(),
               ),
               box(width = NULL, solidHeader = TRUE,
                   title = "Rate of Reported Cases (case/day)",
                   tags$div(HTML(paste(" ", tags$small("Rolling 3-day average"), sep = ""))),
                   leafletOutput("rateMap", height = 500) %>% withSpinner(),
               ),
               box(width = NULL, solidHeader = TRUE,
                   title = tags$div(HTML(paste("Acceleration of Reported Cases (case/day", tags$sup(2), ")", sep = ""))),
                   tags$div(HTML(paste(" ", tags$small("Rolling 3-day average"), sep = ""))),
                   leafletOutput("accelerationMap", height = 500) %>% withSpinner(),
               )
        ),
        column(width = 3,
               
               box(width = NULL, status = "warning",
                   #uiOutput("timeSinceLastUpdate"),
                   p(class = "text-muted",
                     br(),
                     "Data by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", "JHU CSSE", target="_blank"),
                     br(),
                     "Last updated: ", textOutput("lastUpdate"),
                     br(),
                     "Visuals by", tags$a(href="https://shefa.ca", "Shefa Analytics", target="_blank"),

                     
                   )   
                   ),
                   box(width = NULL, status = "warning",
                       #uiOutput("timeSinceLastUpdate"),
 
               p(class = "text-muted",
                 br(),
                 "DISCLAIMER: Strictly intended for research and educational purposes. Visuals are based upon publicly available data which may be prone to errors. Should not be used for medical or policy guidance. Shefa Analytics will not be liable to any person or entity for any loss or damages."
               ))
        )
    )),
    
    tabItem(tabName = "bars",
            fluidRow(
                box(width = NULL, solidHeader = TRUE,
                    title = "Rate of Reported Cases (case/day)",
                    plotOutput("barPlotRate", height = 500) %>% withSpinner()),
                box(width = NULL, solidHeader = TRUE,
                    title = tags$div(HTML(paste("Acceleration of Reported Cases (case/day", tags$sup(2), ")", sep = ""))),
                    plotOutput("barPlotAcceleration", height = 500) %>% withSpinner()),
            )),
  
  tabItem(tabName = "trends",
          fluidRow(
              box(width = NULL, solidHeader = TRUE,
                  title = "Number of Reported Cases",
                  plotOutput("trendPlotCases", height = 500) %>% withSpinner()),
              box(width = NULL, solidHeader = TRUE, 
                  title = "Rate of Reported Cases (case/day)",
                  plotOutput("trendPlotRate", height = 500) %>% withSpinner()),
              box(width = NULL, solidHeader = TRUE,
                  title = tags$div(HTML(paste("Acceleration of Reported Cases (case/day", tags$sup(2), ")", sep = ""))),
                  plotOutput("trendPlotAcceleration", height = 500) %>% withSpinner()),
          ))
  ))


dashboardPage(
    header,
    #dashboardSidebar(disable = TRUE),
    sidebar,
    body
)