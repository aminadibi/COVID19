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
library(patchwork)
library(gghighlight)
library(ftplottools)


url <- "https://docs.google.com/spreadsheets/d/1ad7-09_Jn6AxsdkVPE33T-iLfGpPRmd3piXQqFiVeas/export?&format=csv"

CanadaCases <- read_csv(url)

covidCases <- CanadaCases %>% rename (name = "prname")  %>% rename (Cases = "numconf")  %>% mutate(date=dmy(date)) %>%
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
  filter (name!="Canada") 

colourBlindPal <- c("#000000","#E69F00", "#D55E00", "#999999", "#56B4E9", 
                    "#009E73", "#CC79A7", "#0072B2")      

lineDataCases <- covidCases %>% 
 # pivot_longer(cols = -1, names_to = "date", values_to = "Cases") %>%  
  filter (Cases>=100) %>% arrange (name, date) %>% 
  group_by(name) %>% mutate(days = as.numeric(date - date[1L])) 

lastDay <- max(lineDataCases$days)

pCases <- ggplot(data = lineDataCases, aes(x=days, y=Cases, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 100th cases") + 
  ylab ("Cases \n") +
  # geom_text_repel(data = lineDataCases %>% 
  #                   filter(days == last(days)), aes(label = name, 
  #                                                   x = days + 0.2, 
  #                                                   y = Cases, 
  #                                                   color = name,
  #                                                   fontface=2), size = 5) + 
  gghighlight(name=="QC" | name=="BC" | name == "ON" | name == "AB") +  
  
  scale_y_continuous(trans = log10_trans(),
                     breaks = c(100, 300, 1000, 3000, 10000)) +
 # scale_x_continuous(breaks = c(0:lastDay)) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 100, yend = 100*(2^(1/3))^lastDay,
           colour = "#333333") +
  
  # annotate(geom = "text", x = 5, y = 150, 
  #          label = "doubles every 3 days", color = "#333333", fontface=2,
  #          angle = 20) +
  
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 100, yend = 100*(2^(1/5))^lastDay,
           colour = "#333333") +
  # annotate(geom = "text", x = 5, y = 95, 
  #          label = "doubles every 5 days", color = "#333333", fontface=2,
  #          angle = 13) +
  
  # annotate("segment", linetype = "longdash", 
  #          x = 0, xend = lastDay, y = 50, yend = 50*(2^(1/2))^lastDay,
  #          colour = "#333333") +
  # annotate(geom = "text", x = 4, y = 220, 
  #          label = "doubles every 2 days", color = "#333333", fontface=2,
  #          angle = 30) +
  
  scale_colour_brewer(palette = "Set1") +
  ft_theme() +
  ggtitle(" \n", subtitle = "Cumulative number of confirmed cases") +
  theme(text = element_text(size=16)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) 
  #labs(caption = paste0("Visualization by Shefa Analytics based on a design by John Burn-Murdoch. For more, see shefa.ca. Last updated: ", max(covidCases$date))) 

pTested <- ggplot(data = lineDataCases, aes(x=days, y=numtested, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 100th cases") + 
  ylab ("Tests \n") +
  # geom_text_repel(data = lineDataCases %>% 
  #                   filter(days == last(days)), aes(label = name, 
  #                                                   x = days + 0.2, 
  #                                                   y = numtested, 
  #                                                   color = name,
  #                                                   fontface=2), size = 5) + 
  gghighlight(name=="QC" | name=="BC" | name == "ON" | name == "AB") +  
  
  scale_y_continuous( labels = scales::comma) +
  #scale_x_continuous(breaks = c(0:lastDay)) +

  
  # annotate("segment", linetype = "longdash", 
  #          x = 0, xend = lastDay, y = 50, yend = 50*(2^(1/3))^lastDay,
  #          colour = "#333333") +
  # 
  # annotate(geom = "text", x = 5, y = 150, 
  #          label = "doubles every 3 days", color = "#333333", fontface=2,
  #          angle = 20) +
  # 
  # 
  # annotate("segment", linetype = "longdash", 
  #          x = 0, xend = lastDay, y = 50, yend = 50*(2^(1/5))^lastDay,
  #          colour = "#333333") +
  # annotate(geom = "text", x = 5, y = 95, 
  #          label = "doubles every 5 days", color = "#333333", fontface=2,
  #          angle = 13) +
  
  # annotate("segment", linetype = "longdash", 
  #          x = 0, xend = lastDay, y = 50, yend = 50*(2^(1/2))^lastDay,
  #          colour = "#333333") +
  # annotate(geom = "text", x = 4, y = 220, 
  #          label = "doubles every 2 days", color = "#333333", fontface=2,
  #          angle = 30) +
  
  scale_colour_brewer(palette = "Set1") +
  ft_theme() + 
  ggtitle(" \n", subtitle = "Cumulative number of tests") +
  theme(text = element_text(size=16)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) 
  #labs(caption = paste0("Visualization by Shefa Analytics based on a design by John Burn-Murdoch. For more, see shefa.ca. Last updated: ", max(covidCases$date))) 

lineDataDeaths <- covidCases %>% 
  # pivot_longer(cols = -1, names_to = "date", values_to = "Cases") %>%  
  filter (numdeaths>=10) %>% arrange (name, date) %>% 
  group_by(name) %>% mutate(days = as.numeric(date - date[1L])) 

lastDayDeaths <- max(lineDataDeaths$days)

pDeaths <- ggplot(data = lineDataDeaths, aes(x=days, y=numdeaths, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 10th death") + 
  ylab ("Deaths \n") +
  # geom_text_repel(data = lineDataDeaths %>% 
  #                   filter(days == last(days)), aes(label = name, 
  #                                                   x = days + 0.2, 
  #                                                   y = numdeaths, 
  #                                                   color = name,
  #                                                   fontface=2), size = 5) + 
  
  gghighlight(name=="QC" | name=="BC" | name == "ON" | name == "AB") +  
  
  scale_y_continuous(trans = log10_trans(),
                     breaks = c(10, 20, 50, 100, 200, 500, 1000)) +
  #scale_x_continuous(breaks = c(0:lastDayDeaths)) +
  
   annotate("segment", linetype = "longdash", 
            x = 0, xend = lastDayDeaths, y = 10, yend = 10*(2^(1/3))^lastDayDeaths,
            colour = "#333333") +
   
   # annotate(geom = "text", x = 15, y = 280, 
   #          label = "doubles every 3 days", color = "#333333", fontface=2,
   #          angle = 30) +
  # 
  # 
   annotate("segment", linetype = "longdash", 
            x = 0, xend = lastDayDeaths, y = 10, yend = 10*(2^(1/5))^lastDayDeaths,
            colour = "#333333") +
   # annotate(geom = "text", x = 15, y = 70, 
   #          label = "... every 5 days", color = "#333333", fontface=2,
   #          angle = 18) +
  
  # annotate("segment", linetype = "longdash", 
  #          x = 0, xend = lastDay, y = 50, yend = 50*(2^(1/2))^lastDay,
  #          colour = "#333333") +
  # annotate(geom = "text", x = 4, y = 220, 
  #          label = "doubles every 2 days", color = "#333333", fontface=2,
  #          angle = 30) +
  
  scale_colour_brewer(palette = "Set1") +
  ft_theme() +
  ggtitle(" \n", subtitle = "Cumulative number of deaths") +
  theme(text = element_text(size=16)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) 
  #labs(caption = paste0("Visualization by Shefa Analytics based on a design by John Burn-Murdoch. For more, see shefa.ca. Last updated: ", max(covidCases$date))) 

pCanada <- (pCases / pTested ) | pDeaths +
  labs(caption = paste0("Visualization by Shefa Analytics based on a design by John Burn-Murdoch. For more, see shefa.ca. Last updated: ", max(covidCases$date))) 

pCanada
ggsave(plot = pCanada, "covidcanada.eps", width = 32.2, height = 20, units = "cm", dpi=300)
