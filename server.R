library(shinydashboard)
library(leaflet)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


function(input, output, session) {
    
    getData <-  reactive({
      time_series_19_covid_Confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
      covidCases <- time_series_19_covid_Confirmed %>% rename (name = "Country/Region") %>% group_by(name) %>%
        summarise_at(vars(5:(length(time_series_19_covid_Confirmed)-1)), sum, na.rm = TRUE) %>% mutate(name = replace(name, name == "US", "United States")) %>%
        mutate(name = replace(name, name == "UK", "United Kingdom")) %>% mutate(name = replace(name, name == "Mainland China", "China"))
      return(covidCases)
    })

    output$numVehiclesTable <- renderUI({
        locations <- routeVehicleLocations()
        if (length(locations) == 0 || nrow(locations) == 0)
            return(NULL)
        
        # Create a Bootstrap-styled table
        tags$table(class = "table",
                   tags$thead(tags$tr(
                       tags$th("Color"),
                       tags$th("Direction"),
                       tags$th("Number of vehicles")
                   )),
                   tags$tbody(
                       tags$tr(
                           tags$td(span(style = sprintf(
                               "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                               dirColors[4]
                           ))),
                           tags$td("Northbound"),
                           tags$td(nrow(locations[locations$Direction == "4",]))
                       ),
                       tags$tr(
                           tags$td(span(style = sprintf(
                               "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                               dirColors[1]
                           ))),
                           tags$td("Southbound"),
                           tags$td(nrow(locations[locations$Direction == "1",]))
                       ),
                       tags$tr(
                           tags$td(span(style = sprintf(
                               "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                               dirColors[2]
                           ))),
                           tags$td("Eastbound"),
                           tags$td(nrow(locations[locations$Direction == "2",]))
                       ),
                       tags$tr(
                           tags$td(span(style = sprintf(
                               "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                               dirColors[3]
                           ))),
                           tags$td("Westbound"),
                           tags$td(nrow(locations[locations$Direction == "3",]))
                       ),
                       tags$tr(class = "active",
                               tags$td(),
                               tags$td("Total"),
                               tags$td(nrow(locations))
                       )
                   )
        )
    })
    
    # Store last zoom button value so we can detect when it's clicked
    lastZoomButtonValue <- NULL
    
    derivative <- function(sites) {
        for (i in 1:dim(sites)[1]) {
            for (j in length(sites):6){
                sites[i, j] <- as.numeric(sites[i, j] - sites[i, j-1])
            }  
        }
        return(sites)
    }  
    
    output$accelerationMap <- renderLeaflet({
        
        covidCases <- getData()

        covidAcceleration <- derivative(derivative(covidCases))
        
        # averaging acceleration over the past 3 days
        covidAcceleration[, "threeDayAcceleration"] <- as.vector((covidAcceleration[length(covidAcceleration)] + covidAcceleration[length(covidAcceleration)-1] + covidAcceleration[length(covidAcceleration)-2])/3)
        
        # merging datasets and plotting the map of Coronavirus
        world <- ne_countries(scale = "medium", returnclass = "sf")
        
        covidAccelerationWorld <- world %>% left_join(covidAcceleration)
        
        
        #leaflet map itself
        covidAccelerationWorldLeaf <- covidAccelerationWorld %>% filter (!is.na(threeDayAcceleration))
        map <- leaflet(data = covidAccelerationWorldLeaf) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
        
        # Create a continuous palette function
        pal <- colorNumeric(
            palette = "YlOrRd",
            domain = covidAccelerationWorld$threeDayAcceleration)
        
        # Apply the function to provide RGB colors to addPolygons
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases/day<sup>2</sup>",
            covidAccelerationWorldLeaf$name,  round(covidAccelerationWorldLeaf$threeDayAcceleration, 1)
        ) %>% lapply(htmltools::HTML)
        
        map %>%
            addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
                        fillColor = ~pal(threeDayAcceleration), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
                        highlight = highlightOptions(
                            weight = 0.5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE), 
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"))  %>% 
            addLegend("bottomright", pal = pal, values = ~threeDayAcceleration,
                      title = "Acceleration",
                      opacity = 0.6
            )
        
        # rezoom <- "first"
        # # If zoom button was clicked this time, and store the value, and rezoom
        # if (!identical(lastZoomButtonValue, input$zoomButton)) {
        #     lastZoomButtonValue <<- input$zoomButton
        #     rezoom <- "always"
        # }
        # 
        # map <- map %>% mapOptions(zoomToLimits = rezoom)
        # 
   
    })
    
    output$rateMap <- renderLeaflet({
        
        covidCases <- getData()
        covidRate         <- derivative(covidCases)
        covidRate[, "threeDayRate"] <- as.vector((covidRate[length(covidRate)] + covidRate[length(covidRate)-1] + covidRate[length(covidRate)-2])/3)
        
        # merging datasets and plotting the map of Coronavirus
        world <- ne_countries(scale = "medium", returnclass = "sf")
        covidRateWorld <- world %>% left_join(covidRate)
        covidRateWorldLeaf <- covidRateWorld %>% filter (!is.na(threeDayRate))
        map <- leaflet(data = covidRateWorldLeaf) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
        
        # Create a continuous palette function
        pal <- colorNumeric(
            palette = "YlOrRd",
            domain = covidRateWorld$threeDayRate)
        
        # Apply the function to provide RGB colors to addPolygons
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases/day",
            covidRateWorldLeaf$name,  round(covidRateWorldLeaf$threeDayRate, 1)
        ) %>% lapply(htmltools::HTML)
        
        map %>%
            addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
                        fillColor = ~pal(threeDayRate), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
                        highlight = highlightOptions(
                            weight = 0.5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE), 
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"))  %>% 
            addLegend("bottomright", pal = pal, values = ~threeDayRate,
                      title = "Rate",
                      opacity = 0.6
            )
      
    })
    
    
    output$caseMap <- renderLeaflet({
        
        covidCases <- getData()
        covidCases[, "Cases"] <- as.vector(log(covidCases[length(covidCases)]))
        
        # merging datasets and plotting the map of Coronavirus
        world <- ne_countries(scale = "medium", returnclass = "sf")
        covidCasesWorld <- world %>% left_join(covidCases)
        covidCasesWorldLeaf <- covidCasesWorld %>% filter (!is.na(Cases))
        map <- leaflet(data = covidCasesWorldLeaf) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
        
        # Create a continuous palette function
        pal <- colorNumeric(
            palette = "YlOrRd",
            domain = covidCasesWorld$Cases)
        
        # Apply the function to provide RGB colors to addPolygons
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases",
            covidCasesWorldLeaf$name,  exp(covidCasesWorldLeaf$Cases)
        ) %>% lapply(htmltools::HTML)
        
        map %>%
            addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
                        fillColor = ~pal(Cases), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
                        highlight = highlightOptions(
                            weight = 0.5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE), 
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"))  %>% 
            addLegend("bottomright", pal = pal, values = ~Cases,
                      title = "log(Cases)",
                      opacity = 0.6
            )
    })
    
    output$trendPlots <- renderPlot({
      targetBar <- c("Australia",   
                     "Austria",
                     "Canada",
                     "China",
                     "France",
                     "Germany",
                     "Hong Kong",
                     "Iran",
                     "Italy",
                     "Japan",
                     "Kuwait",
                     "Malaysia",
                     "Netherlands",
                     "Singapore",
                     "South Korea",
                     "Spain",
                     "United Kingdom",
                     "United States")
      
      # plottting the bar chart
      barChartDataAcceleration <- covidAcceleration %>% filter (name %in% targetBar & !is.na(threeDayAcceleration))
      ggplot(data = barChartDataAcceleration) +
        geom_col(aes(y = threeDayAcceleration, x = reorder(name, threeDayAcceleration), fill=threeDayAcceleration)) +
        scale_fill_distiller(type = "div", palette = "RdBu", aesthetics = "fill")+
        coord_flip() + xlab ("") + ylab ("acceleration") + 
        ggtitle("Acceleration of Reported COVID-19 Cases") +
        labs(caption = paste0("(Rolling 3-day average as of ", lubridate::now(), " PST)")) + 
        theme_tufte()
    })
    

}