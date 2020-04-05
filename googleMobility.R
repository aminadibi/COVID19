library(jsonlite)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ftplottools)


Canada <- jsonlite::fromJSON("https://pastelsky.github.io/covid-19-mobility-tracker/output/CA/mobility.json")
US <- jsonlite::fromJSON("https://pastelsky.github.io/covid-19-mobility-tracker/output/US/mobility.json")
GB <- jsonlite::fromJSON("https://pastelsky.github.io/covid-19-mobility-tracker/output/GB/mobility.json")


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
  pivot_longer(cols = -1, names_to = "type", values_to = "values") %>% mutate(country = "UK")

mobility <- bind_rows(canMobility, USMobility, GBMobility)

ggplot(mobility, aes(y=values, x=date, colour = country)) + 
  geom_path (size=1) +
  #scale_color_brewer(palette = "Set1") +
  facet_wrap(~type) +
  ft_theme()


