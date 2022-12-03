##testing animated plot 

library(ggplot2)
library(gganimate)
library(xlsx)
library(dplyr)


date_state_set <- read.xlsx("./Date_State.xlsx", 2)

date_state_set <- date_state_set  %>% 
  filter(State %in% c("CO", "VA", "CA", "WA"))

#dates = read.delim("./dates.txt", header = TRUE, sep = " ")

animited_data <- data.frame (
  date = c(as.Date(date_state_set$Date, format = "%m-%d-%y")),
  states = c(date_state_set$State)
)

##Animated plot
animated_plot <- ggplot(
  animited_data,
  aes(date, states, group = states, color = factor(states))) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Frequency") +
  theme(legend.position = "top")

animated_plot


p <- ggplot(
  airquality,
  aes(Day, Temp, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
p


