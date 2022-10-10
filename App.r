################################
#The amazing data visualization
#project, created by group 13
###############################
#Source: https://data.world/aarranzlopez/ufo-sights-2016-us-and-canada

library(xlsx)

ourdata <- read.xlsx("C:/Users/Emil/Desktop/UNI/DataVisualization/R_opgaver/Project/UFOs_coord-1.xlsx", 1)

summary(ourdata)


plot(ourdata$lat, ourdata$lng, xlab = "Latitude", ylab = "Longitude")