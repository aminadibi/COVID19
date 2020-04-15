library(tidyverse)
library(lubridate)
library(ggthemes)
library(gghighlight)
library(patchwork)
library(ftplottools)

cities <- c("Vancouver",
            "Toronto",
            "Montreal",
            "Ottawa",                    
            "Halifax", 
            "Calgary",
            "Edmonton",
            "New York City", 
            "London",
            "Munich",
            "Seattle",
            "Madrid",
            "Milan",
            "Seoul",
            "Tokyo",
            "Wuhan",
            "Istanbul",
            "San Francisco",
            "Mexico City",
            "Reykjavic",
            "Stockholm",
            "Paris"
            )

cities <- c("Vancouver",
            "Toronto",
            "Montreal",
            "Ottawa",                    
            "Halifax", 
            "Calgary")


cityHighlight <- c("Vancouver",
            "Toronto",
            "Calgary",
            "New York City")


Canada <- read_csv("./applemobilitytrends-2020-04-13.csv")

mobility <- as.data.frame(Canada) %>% filter(region %in% cities) %>% 
  select (-geo_type) %>%
  pivot_longer(cols = -c(1,2), names_to = "date", values_to = "values") %>%
  mutate(date=ymd(date)) %>% mutate(values = values - 100) %>% filter (date>=ymd("2020-03-01"))

p1 <- ggplot(mobility %>% filter(transportation_type == "walking") ) + 
  geom_line (aes(y=values, x=date, colour = region), size=1 ) +
 # geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
  ylab ("% Change") +
  gghighlight(use_direct_label = FALSE) +
  facet_wrap(~ region) +
  ggtitle("Walking")+
  ft_theme() 
  
p2 <- ggplot(mobility %>% filter(transportation_type == "driving") ) + 
     geom_line (aes(y=values, x=date, colour = region), size=1 ) +
   # geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
    ylab ("% Change") +
    gghighlight(use_direct_label = FALSE) +
    facet_wrap(~ region) +
    ggtitle("Driving")+
    ft_theme()   

p3 <- ggplot(mobility %>% filter(transportation_type == "transit") ) + 
     geom_line (aes(y=values, x=date, colour = region), size=1 ) +
    #geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
    ylab ("% Change") +
    gghighlight(use_direct_label = FALSE) +
    facet_wrap(~ region) +
    ggtitle("Transit")+
    ft_theme() 





pCanada <- p1 + p2 + p3 + 
  labs(caption = paste0("Visualization by Shefa Analytics.\nBased on Apple Mobility Trends Data. Last updated: ", "2020-04-13")) &
  theme(legend.position = "none") & xlab("") & scale_x_date(date_labels = "%b", date_breaks = "1 month") 



ggsave(plot = pCanada, "mobilitycanada.eps", width = 32, height = 20, units = "cm", dpi=300)

