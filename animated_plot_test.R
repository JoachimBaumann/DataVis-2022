##testing animated plot 

library(ggplot2)
library(gganimate)
library(xlsx)
library(dplyr)
library(lubridate)
library(scales)


date_state_set <- read.xlsx("./Date_State_Observation.xlsx", 1)


date_state_set <- date_state_set  %>% 
  filter(State %in% c(" CO", " VA", " CA", " WA"))

date_state_set <- date_state_set  %>% 
  filter(Date %in% c(" CO", " VA", " CA", " WA"))


#
#animated_data <- data.frame (
#  date = c(as.Date(date_state_set$Date, format = "%m-%d-%y")),
#  states = c(date_state_set$State), 
#  observations = c(date_state_set$Observations)
#)


##Animated plot
animated_plot <- ggplot(
  date_state_set,
  aes(c(as.Date(date_state_set$Date, format = "%m-%d-%y")), date_state_set$observations
  , group = date_state_set$State, color = factor(date_state_set$State))) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Date", y = "Observations") +
  theme(legend.position = "top") + 
  #scale_x_date(limits = c(as.Date("1-1-16", "12-12-16", format = "%m-%d-%y"))) 
  #scale_x_date(date_breaks = "1 month", limits = as.Date(c("1-1-16", "12-12-16", format = "%m-%d-%y"))) + 
  xlim(as.Date(c("1-1-16", "12-12-16"), format="%m-%d-%y"))
  
animated_plot

graph_animation = animated_plot +
  transition_time(c(as.Date(date_state_set$Date, format = "%m-%d-%y")))



