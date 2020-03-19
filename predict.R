iran <- modelData %>% filter (name == "Iran") 

iranMinusLast <- iran %>%  slice(-n())

library(splines)
library(modelr)
library(tidyverse)

ggplot(iranMinusLast, aes(days=days, y=Cases)) +
  geom_point()

mod1 <- lm(Cases ~ ns(days, 1), data = iranMinusLast)
mod2 <- lm(Cases ~ ns(days, 2), data = iranMinusLast)
mod3 <- lm(Cases ~ ns(days, 3), data = iranMinusLast)
mod4 <- lm(Cases ~ ns(days, 4), data = iranMinusLast)
mod5 <- lm(Cases ~ ns(days, 5), data = iranMinusLast)



grid- iranMinusLast %>% 
  data_grid(days = seq_range(days, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "Cases")

ggplot(iranMinusLast, aes(days, Cases)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)


IranPred <- iran %>% 
  data_grid(days) %>% add_predictions(mod3)



spain<- modelData %>% filter (name == "Spain") 
SpainMinusLast <- spain %>%  slice(-n())



mod1 <- lm(Cases ~ ns(days, 1), data = SpainMinusLast)
mod2 <- lm(Cases ~ ns(days, 2), data = SpainMinusLast)
mod3 <- lm(Cases ~ ns(days, 3), data = SpainMinusLast)
mod4 <- lm(Cases ~ ns(days, 4), data = SpainMinusLast)
mod5 <- lm(Cases ~ ns(days, 5), data = SpainMinusLast)
modlog <- lm(log(Cases)~days, data = SpainMinusLast)
SpainPred<- SpainMinusLast %>% 
  data_grid(days = seq_range(days, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, modlog,  .pred = "Cases")

ggplot(SpainMinusLast, aes(days, Cases)) + 
  geom_point() +
  geom_line(data = SpainPred, colour = "red") +
  facet_wrap(~ model)


SpainPred <- spain %>% 
  data_grid(days) %>% add_predictions(mod3)

