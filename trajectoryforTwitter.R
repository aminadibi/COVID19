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


url <- "https://docs.google.com/spreadsheets/d/1ad7-09_Jn6AxsdkVPE33T-iLfGpPRmd3piXQqFiVeas/export?&format=csv"

CanadaCases <- read_csv(url)

covidCases <- CanadaCases %>% rename (name = "prname")  %>% rename (Cases = "numconf")  %>% mutate(date=dmy(date))

covidCases[258,5] <- 792
colourBlindPal <- c("#000000","#E69F00", "#D55E00", "#999999", "#56B4E9", 
                    "#009E73", "#CC79A7", "#0072B2")      

lineDataCases <- covidCases %>% 
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
  filter (name!="Canada") %>%
 # pivot_longer(cols = -1, names_to = "date", values_to = "Cases") %>%  
  filter (Cases>=50) %>% arrange (name, date) %>% 
  group_by(name) %>% mutate(days = as.numeric(date - date[1L])) 

lastDay <- max(lineDataCases$days)

ggplot(data = lineDataCases, aes(x=days, y=Cases, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 50th cases") + 
  ylab ("Cases \n") +
  geom_text_repel(data = lineDataCases %>% 
                    filter(days == last(days)), aes(label = name, 
                                                    x = days + 0.2, 
                                                    y = Cases, 
                                                    color = name,
                                                    fontface=2), size = 5) + 
  scale_y_continuous(trans = log10_trans(),
                     breaks = c(20, 50, 100, 200, 300, 500, 1000, 2000, 5000, 10000)) +
  scale_x_continuous(breaks = c(0:lastDay)) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 50, yend = 50*(2^(1/3))^lastDay,
           colour = "#333333") +
  
  annotate(geom = "text", x = 5, y = 150, 
           label = "doubles every 3 days", color = "#333333", fontface=2,
           angle = 20) +
  
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 50, yend = 50*(2^(1/5))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 5, y = 95, 
           label = "doubles every 5 days", color = "#333333", fontface=2,
           angle = 13) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 50, yend = 50*(2^(1/2))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 4, y = 220, 
           label = "doubles every 2 days", color = "#333333", fontface=2,
           angle = 30) +
  
  scale_colour_manual(values=colourBlindPal) +
  theme_economist() + 
  ggtitle(" \n", subtitle = "Cumulative number of cases by days since 50th case") +
  theme(text = element_text(size=16)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  labs(caption = paste0("Visualization by Shefa Analytics based on a design by John Burn-Murdoch. For more, see shefa.ca. Last updated: ", max(covidCases$date))) 
ggsave("covidcanada.eps", width = 32.2, height = 20, units = "cm", dpi=300)