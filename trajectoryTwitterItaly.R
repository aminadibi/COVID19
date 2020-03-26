library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(ggrepel)

caseType <- "confirmed_global"
url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_", caseType, ".csv")

time_series_19_covid_Confirmed <- read_csv(url)

covidCases <- time_series_19_covid_Confirmed %>% rename (country = "Country/Region") %>% rename (name = "Province/State") %>%
  filter (country == "Canada" | country == "Italy")  %>% select(-Lat, -Long, -`1/22/20`)
covidCases$name[12] <- "Italy"
sumCanada  <- time_series_19_covid_Confirmed %>% rename (country = "Country/Region") %>% rename (name = "Province/State") %>%
  filter (country == "Canada")  %>%
  group_by(country) %>%
  summarise_at(vars(5:(length(time_series_19_covid_Confirmed)-1)), sum, na.rm = FALSE)

colourBlindPal <- c("#000000","#E69F00", "#D55E00", "#009E73", "#56B4E9", 
                    "#999999", "#CC79A7", "#0072B2")      


sumCanada$name <- "Canada"

lineDataCases <- rbind(covidCases) %>% 
  select (-c(country)) %>% 
  mutate(name = replace(name, name == "British Columbia", "BC")) %>% 
  mutate(name = replace(name, name == "Ontario", "ON")) %>% 
  mutate(name = replace(name, name == "Alberta", "AB")) %>% 
  mutate(name = replace(name, name == "Saskatchewan", "SK")) %>% 
  mutate(name = replace(name, name == "Manitoba", "MB")) %>% 
  mutate(name = replace(name, name == "Quebec", "QC")) %>% 
  mutate(name = replace(name, name == "Nova Scotia", "NS")) %>% 
  mutate(name = replace(name, name == "New Brunswick", "NB")) %>% 
  mutate(name = replace(name, name == "Newfoundland and Labrador", "NL"))%>%
  mutate(name = replace(name, name == "Prince Edward Island", "PE")) %>% 
  mutate(name = replace(name, name == "Yukon", "YT")) %>% 
  mutate(name = replace(name, name == "Northwest Territories", "NT")) %>% 
  mutate(name = replace(name, name == "Nunavut", "NU")) %>% 
  
  pivot_longer(cols = -1, names_to = "date", values_to = "Cases") %>%  mutate(date=mdy(date)) %>%
  filter (Cases>=100) %>% arrange (name, date) %>% 
  group_by(name) %>% mutate(date = date - date[1L]) %>%
  mutate(days = as.numeric(date)) #%>% filter(days <30)

lastDay <- max(lineDataCases$days)

ggplot(data = lineDataCases, aes(x=days, y=Cases, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 100th cases") + 
  ylab ("Cases \n") +
  geom_text_repel(data = lineDataCases %>% 
                    filter(days == last(days)), aes(label = name, 
                                                    x = days + 0.2, 
                                                    y = Cases, 
                                                    color = name,
                                                    fontface=2), size = 5) + 
  #coord_trans(y="log") +
  scale_y_continuous(trans = log10_trans(),
                     breaks = c(20, 50, 100, 200, 300, 500, 1000, 10000, 80000)) +
  scale_x_continuous(breaks = c(0:30)) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 100, yend = 100*(2^(1/3))^lastDay,
           colour = "#333333") +
  
  annotate(geom = "text", x = 23, y = 15000, 
           label = "doubles every 3 days", color = "#333333", fontface=2,
           angle = 18) +
  
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 100, yend = 100*(2^(1/5))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 23, y = 2000, 
           label = "doubles every 5 days", color = "#333333", fontface=2,
           angle = 13) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 100, yend = 100*(2^(1/2))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 23, y = 190000, 
           label = "doubles every 2 days", color = "#333333", fontface=2,
           angle = 20) +
  
  scale_colour_manual(values=colourBlindPal) +
  theme_economist() + 
  ggtitle("March 25th: Quebec on the same trajectory as Italy, 25 days behind\n", subtitle = "Cumulative number of cases by days since 100th case") +
  theme(text = element_text(size=16)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  labs(caption = paste0("Visualization by Shefa Analytics based on a design by John Burn-Murdoch. For more, see shefa.ca. Last updated: ", ymd(mdy(colnames(covidCases[length(covidCases)]))))) 
ggsave("covidcanada.png", width = 32.2, height = 20, units = "cm", dpi=300)
