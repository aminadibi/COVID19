library(shinydashboard)
# library(leaflet)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(ggthemes)
#library(rgeos)
# library(rnaturalearth)
# library(rnaturalearthdata)
library(scales)
library(RColorBrewer)
library(ggrepel)


function(input, output, session) {
    
    #data (state)
    getData <- reactive ({
      caseType <- "Confirmed"
      url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-", caseType, ".csv")
      time_series_19_covid_Confirmed <- read_csv(url)
      
      covidCases <- time_series_19_covid_Confirmed %>% rename (country = "Country/Region") %>% rename (name = "Province/State") %>%
        filter (country == "Canada") 
      
      results <- list()
      results$covidCases <- covidCases
      
      
      # covidAcceleration <- derivative(derivative(covidCases))
      # 
      # # averaging acceleration over the past 3 days
      # covidAcceleration[, "threeDayAcceleration"] <- as.vector((covidAcceleration[length(covidAcceleration)] + covidAcceleration[length(covidAcceleration)-1] + covidAcceleration[length(covidAcceleration)-2])/3)
      # # merging datasets and plotting the map of Coronavirus
      # results$Acceleration <- covidAcceleration
      
      #world <- ne_countries(scale = "medium", returnclass = "sf")
  #    world <- readRDS("world.rds")
      
  #    covidAccelerationWorld <- world %>% left_join(covidAcceleration)
      
  #    results$covidAccelerationWorld <- covidAccelerationWorld
      
      
      # covidRate         <- derivative(covidCases)
      # covidRate[, "threeDayRate"] <- as.vector((covidRate[length(covidRate)] + covidRate[length(covidRate)-1] + covidRate[length(covidRate)-2])/3)
      # results$covidRate <- covidRate
      # merging datasets and plotting the map of Coronavirus
  #    covidRateWorld <- world %>% left_join(covidRate)
  #    results$covidRateWorld <- covidRateWorld
      
      covidCases[, "Cases"] <- as.vector(log(covidCases[length(covidCases)])) 
      covidCases <- covidCases %>% filter(Cases != -Inf)
      
      # merging datasets and plotting the map of Coronavirus
   #   covidCasesWorld <- world %>% left_join(covidCases)
  #    results$covidCasesWorld  <- covidCasesWorld 
      

      
      # plottting the bar chart
      # barChartDataAcceleration <- covidAcceleration %>% filter (!is.na(threeDayAcceleration))
      # 
      # results$barChartDataAcceleration <- barChartDataAcceleration
      # 
      # barChartDataRate <- covidRate %>% filter (!is.na(threeDayRate))
      # results$barChartDataRate<- barChartDataRate
      
      barChartDataCases <- covidCases %>% filter (!is.na(Cases))
      results$barChartDataCases <- barChartDataCases
      return(results)
    })
    
    
    # getDataDeath <- reactive ({
    #   caseType <- "Deaths"
    #   url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-", caseType, ".csv")
    #   time_series_19_covid_Confirmed <- read_csv(url)
    #   
    # })
    
    output$lastUpdate <- renderText({
      
      cases <- getData()$covidCases
      return (colnames(cases[length(cases)]))
      
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
    
    # output$accelerationMap <- renderLeaflet({
    #     
    #     covidAccelerationWorld <- getData()$covidAccelerationWorld
    #     
    #     #leaflet map itself
    #     covidAccelerationWorldLeaf <- covidAccelerationWorld %>% filter (!is.na(threeDayAcceleration))
    #     map <- leaflet(data = covidAccelerationWorldLeaf) %>%
    #         addTiles() %>%  # Add default OpenStreetMap map tiles
    #         addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    #     
    #     # Create a continuous palette function
    #     clrs <- rev(brewer.pal(11, "RdBu"))
    #     scale_range <- c(-1, 1) * max(abs(covidAccelerationWorldLeaf$threeDayAcceleration))
    #      
    #     
    #     pal <- colorNumeric(
    #         palette = clrs,
    #        # domain = covidAccelerationWorld$threeDayAcceleration)
    #        domain = scale_range)
    #     
    #     # Apply the function to provide RGB colors to addPolygons
    #     labels <- sprintf(
    #         "<strong>%s</strong><br/>%g cases/day<sup>2</sup>",
    #         covidAccelerationWorldLeaf$name,  round(covidAccelerationWorldLeaf$threeDayAcceleration, 1)
    #     ) %>% lapply(htmltools::HTML)
    #     
    #     map %>%
    #         addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
    #                     fillColor = ~pal(threeDayAcceleration), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
    #                     highlight = highlightOptions(
    #                         weight = 0.5,
    #                         color = "#666",
    #                         dashArray = "",
    #                         fillOpacity = 0.7,
    #                         bringToFront = TRUE), 
    #                     label = labels,
    #                     labelOptions = labelOptions(
    #                         style = list("font-weight" = "normal", padding = "3px 8px"),
    #                         textsize = "15px",
    #                         direction = "auto"))  %>% 
    #         addLegend("bottomright", pal = pal, values = ~threeDayAcceleration,
    #                   title = "Acceleration",
    #                   opacity = 0.6
    #         ) %>% setView(32, 20, zoom = 1.6)
    # })
    # 
    # output$DeathAccelerationMap <- renderLeaflet({
    #   
    #   covidAccelerationWorld <-  getDataDeath()$covidAccelerationWorld
    #   
    #   #leaflet map itself
    #   covidAccelerationWorldLeaf <- covidAccelerationWorld %>% filter (!is.na(threeDayAcceleration))
    #   map <- leaflet(data = covidAccelerationWorldLeaf) %>%
    #     addTiles() %>%  # Add default OpenStreetMap map tiles
    #     addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    #   
    #   # Create a continuous palette function
    #   clrs <- rev(brewer.pal(11, "RdBu"))
    #   scale_range <- c(-1, 1) * max(abs(covidAccelerationWorldLeaf$threeDayAcceleration))
    #   
    #   
    #   pal <- colorNumeric(
    #     palette = clrs,
    #     # domain = covidAccelerationWorld$threeDayAcceleration)
    #     domain = scale_range)
    #   
    #   # Apply the function to provide RGB colors to addPolygons
    #   labels <- sprintf(
    #     "<strong>%s</strong><br/>%g cases/day<sup>2</sup>",
    #     covidAccelerationWorldLeaf$name,  round(covidAccelerationWorldLeaf$threeDayAcceleration, 1)
    #   ) %>% lapply(htmltools::HTML)
    #   
    #   map %>%
    #     addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
    #                 fillColor = ~pal(threeDayAcceleration), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
    #                 highlight = highlightOptions(
    #                   weight = 0.5,
    #                   color = "#666",
    #                   dashArray = "",
    #                   fillOpacity = 0.7,
    #                   bringToFront = TRUE), 
    #                 label = labels,
    #                 labelOptions = labelOptions(
    #                   style = list("font-weight" = "normal", padding = "3px 8px"),
    #                   textsize = "15px",
    #                   direction = "auto"))  %>% 
    #     addLegend("bottomright", pal = pal, values = ~threeDayAcceleration,
    #               title = "Acceleration",
    #               opacity = 0.6
    #     ) %>% setView(32, 20, zoom = 1.6)
    # })
    # 
    # output$rateMap <- renderLeaflet({
    # 
    #     covidRateWorld <- getData()$covidRateWorld
    #     covidRateWorldLeaf <- covidRateWorld %>% filter (!is.na(threeDayRate))
    #     map <- leaflet(data = covidRateWorldLeaf) %>%
    #         addTiles() %>%  # Add default OpenStreetMap map tiles
    #         addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    #     
    #     # Create a continuous palette function
    #     pal <- colorNumeric(
    #         palette = "YlOrRd",
    #         domain = covidRateWorld$threeDayRate)
    #     
    #     # Apply the function to provide RGB colors to addPolygons
    #     labels <- sprintf(
    #         "<strong>%s</strong><br/>%g cases/day",
    #         covidRateWorldLeaf$name,  round(covidRateWorldLeaf$threeDayRate, 1)
    #     ) %>% lapply(htmltools::HTML)
    #     
    #     map %>%
    #         addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
    #                     fillColor = ~pal(threeDayRate), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
    #                     highlight = highlightOptions(
    #                         weight = 0.5,
    #                         color = "#666",
    #                         dashArray = "",
    #                         fillOpacity = 0.7,
    #                         bringToFront = TRUE), 
    #                     label = labels,
    #                     labelOptions = labelOptions(
    #                         style = list("font-weight" = "normal", padding = "3px 8px"),
    #                         textsize = "15px",
    #                         direction = "auto"))  %>% 
    #         addLegend("bottomright", pal = pal, values = ~threeDayRate,
    #                   title = "Rate",
    #                   opacity = 0.6
    #         )  %>% setView(32, 20, zoom = 1.6)
    #   
    # })
    # 
    # output$deathRateMap <- renderLeaflet({
    #   
    #   covidRateWorld <-  getDataDeath()$covidRateWorld
    #   covidRateWorldLeaf <- covidRateWorld %>% filter (!is.na(threeDayRate))
    #   map <- leaflet(data = covidRateWorldLeaf) %>%
    #     addTiles() %>%  # Add default OpenStreetMap map tiles
    #     addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    #   
    #   # Create a continuous palette function
    #   pal <- colorNumeric(
    #     palette = "YlOrRd",
    #     domain = covidRateWorld$threeDayRate)
    #   
    #   # Apply the function to provide RGB colors to addPolygons
    #   labels <- sprintf(
    #     "<strong>%s</strong><br/>%g cases/day",
    #     covidRateWorldLeaf$name,  round(covidRateWorldLeaf$threeDayRate, 1)
    #   ) %>% lapply(htmltools::HTML)
    #   
    #   map %>%
    #     addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
    #                 fillColor = ~pal(threeDayRate), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
    #                 highlight = highlightOptions(
    #                   weight = 0.5,
    #                   color = "#666",
    #                   dashArray = "",
    #                   fillOpacity = 0.7,
    #                   bringToFront = TRUE), 
    #                 label = labels,
    #                 labelOptions = labelOptions(
    #                   style = list("font-weight" = "normal", padding = "3px 8px"),
    #                   textsize = "15px",
    #                   direction = "auto"))  %>% 
    #     addLegend("bottomright", pal = pal, values = ~threeDayRate,
    #               title = "Rate",
    #               opacity = 0.6
    #     )  %>% setView(32, 20, zoom = 1.6)
    #   
    # })
    # 
    # output$deathMap <- renderLeaflet({
    # 
    #     covidCasesWorld <-  getDataDeath()$covidCasesWorld 
    #     covidCasesWorldLeaf <- covidCasesWorld %>% filter (!is.na(Cases))
    #     map <- leaflet(data = covidCasesWorldLeaf) %>%
    #         addTiles() %>%  # Add default OpenStreetMap map tiles
    #         addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    #     
    #     # Create a continuous palette function
    #     pal <- colorNumeric(
    #         palette = "YlOrRd",
    #         domain = covidCasesWorldLeaf$Cases)
    #     
    #     # Apply the function to provide RGB colors to addPolygons
    #     labels <- sprintf(
    #         "<strong>%s</strong><br/>%g cases",
    #         covidCasesWorldLeaf$name,  exp(covidCasesWorldLeaf$Cases)
    #     ) %>% lapply(htmltools::HTML)
    #     
    #     map %>%
    #         addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
    #                     fillColor = ~pal(Cases), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
    #                     highlight = highlightOptions(
    #                         weight = 0.5,
    #                         color = "#666",
    #                         dashArray = "",
    #                         fillOpacity = 0.7,
    #                         bringToFront = TRUE), 
    #                     label = labels,
    #                     labelOptions = labelOptions(
    #                         style = list("font-weight" = "normal", padding = "3px 8px"),
    #                         textsize = "15px",
    #                         direction = "auto"))  %>% 
    #         addLegend("bottomright", pal = pal, values = ~Cases,
    #                   title = "log(Deaths)",
    #                   opacity = 0.6
    #         )  %>% setView(32, 20, zoom = 1.6)
    # })
    # 
    # output$barPlotAcceleration <- renderPlot({
    #   
    #   barChartDataAcceleration <- getData()$barChartDataAcceleration
    #   ggplot(data = barChartDataAcceleration) +
    #     geom_col(aes(y = threeDayAcceleration, x = reorder(name, threeDayAcceleration), fill=threeDayAcceleration)) +
    #     scale_fill_distiller(type = "div", palette = "RdBu",  limits = c(-1, 1) * max(abs(barChartDataAcceleration$threeDayAcceleration)), aesthetics = "fill")+
    #     coord_flip() + xlab ("") + ylab ("acceleration") + 
    #     #ggtitle("Acceleration of Reported COVID-19 Cases") +
    #     labs(caption = paste0("(Rolling 3-day average as of ", lubridate::now(), " UTC)")) + 
    #     theme_tufte()
    # })
    
    # output$caseMap <- renderLeaflet({
    #   
    #   covidCasesWorld <- getData()$covidCasesWorld  
    #   covidCasesWorldLeaf <- covidCasesWorld %>% filter (!is.na(Cases))
    #   map <- leaflet(data = covidCasesWorldLeaf) %>%
    #     addTiles() %>%  # Add default OpenStreetMap map tiles
    #     addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    #   
    #   # Create a continuous palette function
    #   pal <- colorNumeric(
    #     palette = "YlOrRd",
    #     domain = covidCasesWorld$Cases)
    #   
    #   # Apply the function to provide RGB colors to addPolygons
    #   labels <- sprintf(
    #     "<strong>%s</strong><br/>%g cases",
    #     covidCasesWorldLeaf$name,  exp(covidCasesWorldLeaf$Cases)
    #   ) %>% lapply(htmltools::HTML)
    #   
    #   map %>%
    #     addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
    #                 fillColor = ~pal(Cases), weight = 0.5, color = "white", opacity = 1, dashArray = "3", 
    #                 highlight = highlightOptions(
    #                   weight = 0.5,
    #                   color = "#666",
    #                   dashArray = "",
    #                   fillOpacity = 0.7,
    #                   bringToFront = TRUE), 
    #                 label = labels,
    #                 labelOptions = labelOptions(
    #                   style = list("font-weight" = "normal", padding = "3px 8px"),
    #                   textsize = "15px",
    #                   direction = "auto"))  %>% 
    #     addLegend("bottomright", pal = pal, values = ~Cases,
    #               title = "log(Cases)",
    #               opacity = 0.6
    #     )  %>% setView(32, 20, zoom = 1.6)
    # })
    # 
    # output$barPlotAcceleration <- renderPlot({
      
    #   barChartDataAcceleration <- getData()$barChartDataAcceleration
    #   ggplot(data = barChartDataAcceleration) +
    #     geom_col(aes(y = threeDayAcceleration, x = reorder(name, threeDayAcceleration), fill=threeDayAcceleration)) +
    #     scale_fill_distiller(type = "div", palette = "RdBu",  limits = c(-1, 1) * max(abs(barChartDataAcceleration$threeDayAcceleration)), aesthetics = "fill")+
    #     coord_flip() + xlab ("") + ylab ("acceleration") + 
    #     #ggtitle("Acceleration of Reported COVID-19 Cases") +
    #     labs(caption = paste0("(Rolling 3-day average as of ", lubridate::now(), " UTC)")) + 
    #     theme_tufte()
    # })
    
    # 
    # output$barPlotRate <- renderPlot({
    #   
    #   # plottting the bar chart
    #   barChartDataRate <- getData()$barChartDataRate
    #   
    #   ggplot(data = barChartDataRate) +
    #     geom_col(aes(y = threeDayRate, x = reorder(name, threeDayRate), fill=threeDayRate)) +
    #     scale_fill_distiller(type = "div", palette = "RdYlBu", aesthetics = "fill")+
    #     coord_flip() + xlab ("") + ylab ("Rate") + 
    #    # ggtitle("Rate of Reported COVID-19 Cases") +
    #     labs(caption = paste0("(Rolling 3-day average as of ", lubridate::now(), " UTC)")) + 
    #     theme_tufte()
    # })
    
    # 
    # output$trendPlotAcceleration <- renderPlot({
    #   
    #   # Plotting accelration over time
    #  
    #   targetLine <- c("Canada", 
    #                   "China",
    #                   "France",
    #                   "Germany",
    #                   "Iran",
    #                   "Italy",
    #                   "Korea, South",
    #                   "United States")
    #   
    #   # The palette with black:
    #   colourBlindPal <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
    #                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    #   
    #   rollingAvg <- function(data) {
    #     for (i in 1:dim(data)[1]) {
    #       for (j in length(data):4){
    #         data[i, j] <- as.numeric(((data[i, j] + data[i, j-1] + data[i, j-2])/3))
    #       }  
    #     }
    #     return(data)
    #   } 
    #   
    #   lineDataAcceleration <- rollingAvg(getData()$barChartDataAcceleration) %>% select(-threeDayAcceleration) %>% pivot_longer(cols = -1, names_to = "date", values_to = "acceleration") %>% mutate(date=mdy(date)) %>%filter(date>"2020-02-21" & name %in% targetLine)
    #   
    #   ggplot(data = lineDataAcceleration, aes(x=date, y=acceleration, colour = name)) +
    #     geom_line(size=1) + xlab ("") + ylab ("cases/day^2") +
    #    # ggtitle("Acceleration of Reported COVID-19 Cases") + 
    #     labs(caption = paste0("(Rolling 3-day average as of ", lubridate::now(), " UTC)")) + 
    #     scale_colour_manual(values=colourBlindPal) +
    #     theme_tufte() +
    #     theme(legend.title=element_blank()) 
    #   
    # })
    # 
    # output$trendPlotRate <- renderPlot({
    #   
    #   targetLine <- c("Canada", 
    #                   "China",
    #                   "France",
    #                   "Germany",
    #                   "Iran",
    #                   "Italy",
    #                   "Korea, South",
    #                   "United States")
    #   
    #   # The palette with black:
    #   colourBlindPal <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
    #                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    #   
    #   rollingAvg <- function(data) {
    #     for (i in 1:dim(data)[1]) {
    #       for (j in length(data):4){
    #         data[i, j] <- as.numeric(((data[i, j] + data[i, j-1] + data[i, j-2])/3))
    #       }  
    #     }
    #     return(data)
    #   } 
    #   
    #   lineDataRate <- rollingAvg(getData()$barChartDataRate) %>% select(-threeDayRate) %>% pivot_longer(cols = -1, names_to = "date", values_to = "Rate") %>% mutate(date=mdy(date)) %>%filter(date>"2020-02-21" & name %in% targetLine)
    #   
    #   ggplot(data = lineDataRate, aes(x=date, y=Rate, colour = name)) +
    #     geom_line(size=1) + xlab ("") + ylab ("cases/day") +
    #   #  ggtitle("Rate of Reported COVID-19 Cases") + 
    #     labs(caption = paste0("(Rolling 3-day average as of ", lubridate::now(), " UTC)")) + 
    #     scale_colour_manual(values=colourBlindPal) +
    #     theme_tufte() + 
    #     theme(legend.title=element_blank())
    #   
    # })
    # 
    # output$epiCurve<- renderPlot({
    #   targetCountry <- input$countryInput
    #   targetCountry2 <- input$countryInput2
    #   
    #   epicCurveData <- getData()$covidRate %>% select(-threeDayRate) %>% pivot_longer(cols = -1, names_to = "date", values_to = "NewCases") %>% mutate(date=mdy(date)) %>%filter(name %in% c(targetCountry, targetCountry2) )
    #   
    #   ggplot (data = epicCurveData, aes(x=date, y=NewCases, fill = name)) +
    #     geom_col(size=1, position = "dodge") + xlab ("") + ylab ("cases/day") +
    #     #scale_colour_manual(values=colourBlindPal) +
    #     theme_tufte() + 
    #     theme(legend.title=element_blank())
    #     
    # })
    # 
    output$compareEpi <- renderPlot({

      # The palette with black:
      colourBlindPal <- c("#000000","#E69F00", "#D55E00", "#009E73", "#56B4E9", 
                          "#999999", "#CC79A7", "#0072B2")   
      cases <- getData()$covidCases
      
      lineDataCases <- getData()$barChartDataCases %>% 
        select(-Cases) %>% select (-c(country, Lat, Long)) %>% 
        mutate(name = replace(name, name == "British Columbia", "BC")) %>% 
        mutate(name = replace(name, name == "Ontario", "ON")) %>% 
        mutate(name = replace(name, name == "Alberta", "AB")) %>% 
        mutate(name = replace(name, name == "Saskatchewan", "SK")) %>% 
        mutate(name = replace(name, name == "Manitoba", "MB")) %>% 
        mutate(name = replace(name, name == "Quebec", "QC")) %>% 
        mutate(name = replace(name, name == "Nova Scotia", "NS")) %>% 
        mutate(name = replace(name, name == "New Brunswick", "NB")) %>% 

        mutate(name = replace(name, name == "Newfoundland and Labrador", "NL")) %>% 
        mutate(name = replace(name, name == "Prince Edward Island", "PE")) %>% 
        mutate(name = replace(name, name == "Yukon", "YT")) %>% 
        mutate(name = replace(name, name == "Northwest Territories", "NT")) %>% 
        mutate(name = replace(name, name == "Nunavut", "NU")) %>% 
        
        pivot_longer(cols = -1, names_to = "date", values_to = "Cases") %>%  mutate(date=mdy(date)) %>%
        filter (Cases>=50) %>% arrange (name, date) %>% group_by(name) %>% mutate(date = date - date[1L]) %>%
        mutate(days = as.numeric(date)) #%>% filter(days <30)
      
      ggplot(data = lineDataCases, aes(x=days, y=Cases, colour = name)) +
        geom_line(size=0.7) + geom_point(size=1) + xlab ("\n Number of days since 50th cases") + 
        ylab ("Cases \n") +
        geom_text_repel(data = lineDataCases %>% 
                          filter(days == last(days)), aes(label = name, 
                                                          x = days + 0.2, 
                                                          y = Cases, 
                                                          color = name,
                                                          fontface=2), size = 5) + 
        coord_trans(y="log") +
        scale_y_continuous(trans = log10_trans(),
                           breaks = c(20, 50, 100, 200, 300, 500, 1000)) +
        scale_x_continuous(breaks = c(0:10)) +
        annotate("segment", linetype = "longdash", 
                 x = 0, xend = 10, y = 50, yend = 866,
                 colour = "#333333") +
        annotate(geom = "text", x = 8, y = 580, 
                 label = "33% daily increase", color = "#333333", fontface=2,
                 angle = 23) +
        scale_colour_manual(values=colourBlindPal) +
        theme_economist() + 
        ggtitle("Alberta & Quebec are about 3 days behind BC & Ontario\nfollowing along a similar trajectory \n", subtitle = "Cumulative number of cases by days since 50th case") +
        theme(text = element_text(size=16)) +
        theme(legend.position = "none") +
        theme(legend.title=element_blank()) +
        labs(caption = paste0("Last updated: ", colnames(cases[length(cases)])))  
      
        #ggsave("covidcanada.png", dpi=300)
      
    })
    
    
    # output$compareEpiDeath <- renderPlot({
    #   
    #   targetCompare <- c("Canada", 
    #                      "China",
    #                      "France",
    #                      "Spain",
    #                      "Germany",
    #                      "Iran",
    #                      "Italy",
    #                      "Korea, South",
    #                      "Singapore",
    #                      "Japan",
    #                      "United States",
    #                      "United Kingdom")
    #   
    #   # The palette with black:
    #   colourBlindPal <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
    #                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    #   
    #   lineDataCases <- getDataDeath()$barChartDataCases %>% 
    #     select(-Cases) %>%  pivot_longer(cols = -1, names_to = "date", values_to = "Cases") %>%  mutate(date=mdy(date)) %>%
    #     filter (Cases>10) %>% arrange (name, date) %>% group_by(name) %>% mutate(date = date - date[1L]) %>%
    #     mutate(days = as.numeric(date)) %>% mutate_if(is.factor, as.character) %>% filter(days <30) # %>% filter(name %in% targetCompare)
    #   
    #   ggplot(data = lineDataCases, aes(x=days, y=Cases, colour = name)) +
    #     geom_line(size=1) + xlab ("number of days since 10th death") + ylab ("deaths") +
    #     geom_text(data = lineDataCases %>% filter(days == last(days)), aes(label = name, 
    #                                                                        x = days + 1, 
    #                                                                        y = Cases, 
    #                                                                        color = name)) + 
    #     # scale_y_continuous(
    #     #                    breaks = seq(0, 50000, by = 5000)) +
    #     coord_trans(y="log") +
    #     scale_y_continuous(trans = log10_trans(),
    #                        breaks = c(20, 50, 100, 200, 300, 500, 1000, 5000),
    #                        # breaks = trans_breaks("log10", function(x) 10^x),
    #                        # labels = trans_format("log10", math_format(10^.x))
    #     ) +
    #     #scale_colour_manual(values=colourBlindPal) +
    #     theme_tufte() + 
    #     theme(legend.position = "none") +
    #     theme(legend.title=element_blank()) +
    #     labs(caption = paste0("(as of ", lubridate::now(), " UTC)"))  
    #   
    #   
    # })
    # 
    # output$trendPlotCases <- renderPlot({
    #   
    #   targetLine <- c("Canada", 
    #                   "China",
    #                   "France",
    #                   "Germany",
    #                   "Iran",
    #                   "Italy",
    #                   "Korea, South",
    #                   "United States")
    #   
    #   # The palette with black:
    #   colourBlindPal <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
    #                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    #   
    #   lineDataCases <- getData()$barChartDataCases %>% select(-Cases) %>% 
    #     pivot_longer(cols = -1, names_to = "date", values_to = "Cases") %>% 
    #     mutate(date=mdy(date)) %>%filter(date>"2020-02-18" & name %in% targetLine)
    #   
    #   ggplot(data = lineDataCases, aes(x=date, y=Cases, colour = name)) +
    #     geom_line(size=1) + xlab ("") + ylab ("cases") +
    #   #  ggtitle("Reported COVID-19 Cases") + 
    #     labs(caption = paste0("(as of ", lubridate::now(), " UTC)")) + 
    #     coord_trans(y="log") +
    #     scale_y_continuous(trans = log10_trans(),
    #                        breaks = trans_breaks("log10", function(x) 10^x),
    #                        labels = trans_format("log10", math_format(10^.x))) +
    #     scale_colour_manual(values=colourBlindPal) +
    #     theme_tufte() + 
    #     theme(legend.title=element_blank())
    #   
    # })
    # 
    # 
    # 

}