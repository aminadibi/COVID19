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


cityHighlight <- c("Vancouver",
            "Toronto",
            "Calgary",
            "New York City")


Canada <- read_csv("./applemobilitytrends-2020-04-13.csv")

mobility <- as.data.frame(Canada) %>% filter(region %in% cities) %>% 
  select (-geo_type) %>%
  pivot_longer(cols = -c(1,2), names_to = "date", values_to = "values") %>%
  mutate(date=ymd(date))

ggplot(mobility %>% filter(transportation_type == "walking") ) + 
  geom_line (aes(y=values, x=date, colour = region), size=1 ) +
  #geom_smooth (aes(y=values, x=date, colour = region), size=1, se=F ) +
  gghighlight() +
  facet_wrap(~ region) +
  theme_tufte() 



  facet_wrap(~transportation_type) 


p1 <- ggplot(mobility %>% filter(type == "parks") ) + 
  geom_line (aes(y=values, x=date, colour = country), size=1) +
  gghighlight(country=="Canada") +  
  #scale_color_brewer(palette = "Set1") +
  # facet_wrap(~type) +
  ft_theme()

p2 <- ggplot(mobility %>% filter(type == "transitStations")) + 
  geom_line (aes(y=values, x=date, colour = country), size=1) +
  gghighlight(country=="Canada" | country=="GB" | country == "SE") +  
  #scale_color_brewer(palette = "Set1") +
  # facet_wrap(~type) +
  ft_theme()

p1 + p2
