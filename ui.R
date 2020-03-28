library(shinydashboard)
#library(leaflet)
library(shinycssloaders)
library(dplyr)

#countryNames <- readRDS("countryNames.rds")
header <- dashboardHeader(
    title = "COVID-19 in Canada"
)

sidebar <-    dashboardSidebar(
    sidebarMenu(
        menuItem("Compare Growth", tabName = "growth")
#        menuItem("Latest Maps", tabName = "dashboard"),
#        menuItem("Epi Curves", tabName = "countries"),
#        menuItem("Bars", tabName = "bars"),
#        menuItem("Trends", tabName = "trends")
        
#        menuItem("Raw data", tabName = "rawdata")
    )
)

body <- dashboardBody(
    tabItems(
        # First tab content
  # tabItem(tabName = "dashboard",
  #   fluidRow(
  #       column(width = 9,
  #              box(width = NULL, solidHeader = TRUE,
  #                  title = "Number of Reported Cases",
  #                  leafletOutput("caseMap", height = 500) %>% withSpinner(),
  #              ),
  #              box(width = NULL, solidHeader = TRUE,
  #                  title = "Rate of Reported Cases (case/day)",
  #                  tags$div(HTML(paste(" ", tags$small("Rolling 3-day average"), sep = ""))),
  #                  leafletOutput("rateMap", height = 500) %>% withSpinner(),
  #              ),
  #              box(width = NULL, solidHeader = TRUE,
  #                  title = tags$div(HTML(paste("Acceleration of Reported Cases (case/day", tags$sup(2), ")", sep = ""))),
  #                  tags$div(HTML(paste(" ", tags$small("Rolling 3-day average"), sep = ""))),
  #                  leafletOutput("accelerationMap", height = 500) %>% withSpinner(),
  #              ),
  #              box(width = NULL, solidHeader = TRUE,
  #                  title = "Number of Reported Deaths",
  #                  leafletOutput("deathMap", height = 500) %>% withSpinner(),
  #              ),
  #              box(width = NULL, solidHeader = TRUE,
  #                  title = "Rate of Reported Deaths (case/day)",
  #                  tags$div(HTML(paste(" ", tags$small("Rolling 3-day average"), sep = ""))),
  #                  leafletOutput("deathRateMap", height = 500) %>% withSpinner(),
  #              ),
  #              box(width = NULL, solidHeader = TRUE,
  #                  title = tags$div(HTML(paste("Acceleration of Reported Deaths (case/day", tags$sup(2), ")", sep = ""))),
  #                  tags$div(HTML(paste(" ", tags$small("Rolling 3-day average"), sep = ""))),
  #                  leafletOutput("DeathAccelerationMap", height = 500) %>% withSpinner(),
  #              )
  #       ),
  #       column(width = 3,
  #              
  #              box(width = NULL, status = "warning",
  #                  #uiOutput("timeSinceLastUpdate"),
  #                  p(class = "text-muted",
  #                    br(),
  #                    "Data by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", "JHU CSSE", target="_blank"),
  #                    br(),
  #                    "Last updated: ", textOutput("lastUpdate"),
  #                    br(),
  #                    "Visuals by", tags$a(href="https://shefa.ca", "Shefa Analytics", target="_blank"),
  # 
  #                    
  #                  )   
  #                  ),
  #                  box(width = NULL, status = "warning",
  #                      #uiOutput("timeSinceLastUpdate"),
  # 
  #              p(class = "text-muted",
  #                br(),
  #                "DISCLAIMER: Strictly intended for research and educational purposes. Visuals are based upon publicly available data which may be prone to errors. Should not be used for medical or policy guidance. Shefa Analytics will not be liable to any person or entity for any loss or damages."
  #              ))
  #       )
  #   )),
  #   
  #   tabItem(tabName = "bars",
  #           fluidRow(
  #               box(width = NULL, solidHeader = TRUE,
  #                   title = "Rate of Reported Cases (case/day)",
  #                   plotOutput("barPlotRate", height = 500) %>% withSpinner()),
  #               box(width = NULL, solidHeader = TRUE,
  #                   title = tags$div(HTML(paste("Acceleration of Reported Cases (case/day", tags$sup(2), ")", sep = ""))),
  #                   plotOutput("barPlotAcceleration", height = 500) %>% withSpinner()),
  #           )),
  # 
  # tabItem(tabName = "trends",
  #         fluidRow(
  #             box(width = NULL, solidHeader = TRUE,
  #                 title = "Number of Reported Cases",
  #                 plotOutput("trendPlotCases", height = 500) %>% withSpinner()),
  #             box(width = NULL, solidHeader = TRUE, 
  #                 title = "Rate of Reported Cases (case/day)",
  #                 plotOutput("trendPlotRate", height = 500) %>% withSpinner()),
  #             box(width = NULL, solidHeader = TRUE,
  #                 title = tags$div(HTML(paste("Acceleration of Reported Cases (case/day", tags$sup(2), ")", sep = ""))),
  #                 plotOutput("trendPlotAcceleration", height = 500) %>% withSpinner()),
  #         )),
  # 
  # tabItem(tabName = "countries",
  #         fluidRow(
  #           box(width = NULL, solidHeader = TRUE,
  #               title = "Epidemic Curve",
  #               selectInput("countryInput", "Country",
  #                           choices = countryNames, selected = "Iran"),
  #               selectInput("countryInput2", "Second Country",
  #                           choices = countryNames, selected = "Italy"),
  #               plotOutput("epiCurve", height = 500) %>% withSpinner())
  #         )),
  
  tabItem(tabName = "growth",
          fluidRow(
            box(width = NULL, solidHeader = TRUE,
                #title = "Comparing Trends ",
                plotOutput("compareEpi", height = 500) %>% withSpinner(),
                #plotOutput("compareEpiDeath", height = 500) %>% withSpinner()
     
            box(width = NULL, status = "warning",
                #uiOutput("timeSinceLastUpdate"),
                p(class = "text-muted",
                #  br(),
                  "Data by ", tags$a(href="https://www.canada.ca/en/public-health/services/diseases/coronavirus-disease-covid-19.html", "Canada.ca", target="_blank"),
                  br(),
                #  "Last updated: ", textOutput("lastUpdate"),
                  #br(),
                  "Visuals by", tags$a(href="https://shefa.ca", "Shefa Analytics", target="_blank"), ". Design adopted from the trajectory tracker by ", tags$a(href="https://twitter.com/jburnmurdoch/", "John Burn-Murdoch", target="_blank"),
                  br(),  
                  "DISCLAIMER: Strictly intended for research and educational purposes. Visuals are based upon publicly available data which may be prone to errors. Should not be used for medical or policy guidance. Shefa Analytics will not be liable to any person or entity for any loss or damages."
                )   
                )

            )
          ))
  ))


dashboardPage(
    header,
    #dashboardSidebar(disable = TRUE),
    sidebar,
    body
)