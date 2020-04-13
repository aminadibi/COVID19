library(jsonlite)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gghighlight)
library(patchwork)
library(ftplottools)

Canada <- jsonlite::fromJSON("https://pastelsky.github.io/covid-19-mobility-tracker/output/CA/mobility.json")
US <- jsonlite::fromJSON("https://pastelsky.github.io/covid-19-mobility-tracker/output/US/mobility.json")
GB <- jsonlite::fromJSON("https://pastelsky.github.io/covid-19-mobility-tracker/output/GB/mobility.json")
SE <- jsonlite::fromJSON("https://pastelsky.github.io/covid-19-mobility-tracker/output/SE/mobility.json")
#KR <- jsonlite::fromJSON("https://pastelsky.github.io/covid-19-mobility-tracker/output/KR/mobility.json")


canMobility <- as.data.frame(Canada$country) %>%  rename (date = "retailAndRecreation.points.date") %>% 
               mutate (date = ymd(date)) %>% select (-ends_with(".date")) %>%
                rename_all(
                  ~stringr::str_replace_all(., ".points.value", "")
                ) %>%
               pivot_longer(cols = -1, names_to = "type", values_to = "values") %>% mutate(country = "Canada")

USMobility <- as.data.frame(US$country) %>%  rename (date = "retailAndRecreation.points.date") %>% 
  mutate (date = ymd(date)) %>% select (-ends_with(".date")) %>%
  rename_all(
    ~stringr::str_replace_all(., ".points.value", "")
  ) %>%
  pivot_longer(cols = -1, names_to = "type", values_to = "values") %>% mutate(country = "US")

GBMobility <- as.data.frame(GB$country) %>%  rename (date = "retailAndRecreation.points.date") %>% 
  mutate (date = ymd(date)) %>% select (-ends_with(".date")) %>%
  rename_all(
    ~stringr::str_replace_all(., ".points.value", "")
  ) %>%
  pivot_longer(cols = -1, names_to = "type", values_to = "values") %>% mutate(country = "GB")


SEMobility <- as.data.frame(SE$country) %>%  rename (date = "retailAndRecreation.points.date") %>% 
  mutate (date = ymd(date)) %>% select (-ends_with(".date")) %>%
  rename_all(
    ~stringr::str_replace_all(., ".points.value", "")
  ) %>%
  pivot_longer(cols = -1, names_to = "type", values_to = "values") %>% mutate(country = "SE")


#KRMobility <- as.data.frame(KR$country) %>%  rename (date = "retailAndRecreation.points.date") %>% 
  # mutate (date = ymd(date)) %>% select (-ends_with(".date")) %>%
  # rename_all(
  #   ~stringr::str_replace_all(., ".points.value", "")
  # ) %>%
  # pivot_longer(cols = -1, names_to = "type", values_to = "values") %>% mutate(country = "KR")

mobility <- bind_rows(canMobility, USMobility, GBMobility, SEMobility) #%>% filter(type == "parks")


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
