##testing animated plot 

library(ggplot2)
library(gganimate)
library(xlsx)
library(dplyr)
library(lubridate)
library(scales)


date_state_set <- read.xlsx("./Date_State_Observation.xlsx", 1)

month_state_set <- read.xlsx("./month_state_observation.xlsx", 1)

month_state_set <- month_state_set  %>% 
  filter(State %in% c("  AZ", "  CA", "  CO", "  WA"))

date_state_set <- date_state_set  %>% 
  filter(State %in% c(" AZ", " CA"))


animated_data <- data.frame (
  dates = c(as.Date(date_state_set$Date, format = "%m-%d-%y")),
  states = c(date_state_set$State), 
  observations = c(date_state_set$Observations)
)


animated_data_month <- data.frame (
  month = c(as.integer(month_state_set$Month)),
  states = c(month_state_set$State), 
  observations = c(month_state_set$Observation)
)


animated_plot <- ggplot(
  animated_data,
  aes(dates, observations, group = states, color = factor(states))
) +
  geom_line() +  
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Date", y = "Observations in state") +
  theme(legend.position = "top") + 
  xlim(as.Date(c("1-1-16", "12-12-16"), format="%m-%d-%y"))

animated_plot

#Does not work as intended
animated_plot + 
  geom_point() +
  geom_line() + 
  transition_reveal(along = dates)


##Animated plot
animated_plot_month <- ggplot(
  animated_data_month,
  aes(month, observations, group = states, color = factor(states))
) +
  geom_line() +  
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Month of the Year", y = "Observations in state") +
  theme(legend.position = "top")

animated_plot_month

animated_plot_month + transition_reveal(month)



