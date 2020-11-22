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
            # "Edmonton",
            # "New York City",
            # "Seattle")


cityHighlight <- c("Vancouver",
            "Toronto",
            "Calgary",
            "New York City",
            "Seattle")


appleData <- read_csv("./applemobilitytrends-2020-08-17.csv")

mobility <- as.data.frame(appleData) %>% filter(region %in% cities) %>% 
  select (-geo_type, -alternative_name, -`sub-region`, -country) %>%
  pivot_longer(cols = -c(1,2), names_to = "date", values_to = "values") %>%
  mutate(date=ymd(date)) %>% mutate(values = values - 100) %>% filter (date>=ymd("2020-03-13"))

p1 <- ggplot(mobility %>% filter(transportation_type == "walking") ) + 
  #geom_line (aes(y=values, x=date, colour = region), size=1 ) +
  geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
  #geom_vline(xintercept=ymd("2020-03-20"), linetype="dashed", color = "red") + 
  ylab ("% Change") +
  gghighlight(use_direct_label = FALSE) +
  facet_wrap(~ region) +
  ggtitle("Walking")+
  ft_theme() 
  
p2 <- ggplot(mobility %>% filter(transportation_type == "driving") ) + 
   #  geom_line (aes(y=values, x=date, colour = region), size=1 ) +
    geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
    ylab ("% Change") +
    gghighlight(use_direct_label = FALSE) +
    facet_wrap(~ region) +
    ggtitle("Driving")+
    ft_theme()   

p3 <- ggplot(mobility %>% filter(transportation_type == "transit") ) + 
    # geom_line (aes(y=values, x=date, colour = region), size=1 ) +
    geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
    ylab ("% Change") +
    gghighlight(use_direct_label = FALSE) +
    facet_wrap(~ region) +
    ggtitle("Transit")+
    ft_theme() 





pCanada <- p1 + p2 + p3 + 
  labs(caption = paste0("Visualization by Shefa Analytics.\nBased on Apple Mobility Trends Data. Last updated: ", lubridate::today())) &
  theme(legend.position = "none") & xlab("") & scale_x_date(date_labels = "%m", date_breaks = "1 month") 



ggsave(plot = pCanada, "mobilitycanada.pdf", width = 42, height = 20, units = "cm", dpi=300)

ggsave(plot = pCanada, "mobilitycanada2.png", width = 42, height = 20, units = "cm", dpi=300)


#### WORLD

exclusion <- c("Albania",
               "Estonia",
               "Israel",
               "Macao",                    
               "United Arab Emirates", 
               "Latvia",
               
               "Croatia")
  
mobilityWorld <- as.data.frame(appleData) %>% filter(geo_type == "country/region") %>% 
  select (-geo_type, -alternative_name, -`sub-region`, -country) %>%
  pivot_longer(cols = -c(1,2), names_to = "date", values_to = "values") %>%
  mutate(date=ymd(date)) %>% mutate(values = values - 100) %>% filter (date>=ymd("2020-03-15") & !region %in% exclusion) 

p1w <- ggplot(mobilityWorld %>% filter(transportation_type == "walking") ) + 
  geom_line (aes(y=values, x=date, colour = region), size=1 ) +
  # geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
  ylab ("% Change") +
  gghighlight(use_direct_label = FALSE) +
  facet_wrap(~ region) +
  ggtitle("Walking")+
  ft_theme() +   
  labs(caption = paste0("Visualization by Shefa Analytics.\nBased on Apple Mobility Trends Data. Last updated: ", "2020-04-20")) +
  theme(legend.position = "none") + xlab("") + scale_x_date(date_labels = "%b", date_breaks = "1 month") 

p2w <- ggplot(mobilityWorld %>% filter(transportation_type == "driving") ) + 
  geom_line (aes(y=values, x=date, colour = region), size=1 ) +
  # geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
  ylab ("% Change") +
  gghighlight(use_direct_label = FALSE) +
  facet_wrap(~ region) +
  ggtitle("driving")+
  ft_theme() +   
  labs(caption = paste0("Visualization by Shefa Analytics.\nBased on Apple Mobility Trends Data. Last updated: ", "2020-04-20")) +
  theme(legend.position = "none") + xlab("") + scale_x_date(date_labels = "%b", date_breaks = "1 month") 


p3w <- ggplot(mobilityWorld %>% filter(transportation_type == "transit") ) + 
  geom_line (aes(y=values, x=date, colour = region), size=1 ) +
  # geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
  ylab ("% Change") +
  gghighlight(use_direct_label = FALSE) +
  facet_wrap(~ region) +
  ggtitle("Transit")+
  ft_theme() +   
  labs(caption = paste0("Visualization by Shefa Analytics.\nBased on Apple Mobility Trends Data. Last updated: ", "2020-04-20")) +
  theme(legend.position = "none") + xlab("") + scale_x_date(date_labels = "%b", date_breaks = "1 month") 

ggsave(plot = p1w, "walking world.pdf", width = 279.4 , height = 215.9, units = "mm", dpi=300)
ggsave(plot = p2w, "driving world.pdf", width = 279.4 , height = 215.9, units = "mm", dpi=300)
ggsave(plot = p3w, "transit world.pdf", width = 279.4 , height = 215.9, units = "mm", dpi=300)
