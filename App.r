################################
#The amazing data visualization
#project, created by group 13
###############################
#Source: https://data.world/aarranzlopez/ufo-sights-2016-us-and-canada

library(xlsx)
library(tidyverse)
library(sf)
library(mapview)
library(viridis)
library(RColorBrewer)


ourdata <- read.xlsx("./UFOs_coord-1.xlsx", 1)

#summary(ourdata)
ourdata %>% 
  glimpse()


##Filter to get only state with California. 
#ourdata <- ourdata  %>% 
#  filter(State == "CA")

#plot(ourdata$lat, ourdata$lng, xlab = "Latitude", ylab = "Longitude")


##This is to visualize the different locations on a map. 
mapview(ourdata, xcol = "lng", ycol = "lat", crs = 4269, grid = FALSE)

colfunc <- colorRampPalette(c("white", "red"))

counts_shape <- table(ourdata$Shape)
barplot_shapes <- barplot(counts_shape, main="Shape distribution",
        xlab="Shapes observed", col=colfunc(30) , beside=False)


counts_state <- table(ourdata$State)
barplot_shapes <- barplot(counts_state, main="State distribution",
                          xlab="Observations in states", col=colfunc(60) , beside=False)


