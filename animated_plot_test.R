##testing animated plot 

library(ggplot2)
library(gganimate)
library(xlsx)


ourdata <- read.xlsx("./UFOs_coord-1.xlsx", 1)
date_state_set <- read.xlsx(".Date_State.xlsx", 1)


dates = read.delim("./dates.txt", header = TRUE, sep = " ")
counts_state <- table(ourdata$State)
animited_data <- data.frame (
  date = c(as.Date(unlist(dates), format = "%m-%d-%y")),
  states = c(counts_state)
)

##Animated plot
animated_plot <- ggplot(
  ourdata$State,
  aes(ourdata$Date...Time, counts_state)) +
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


