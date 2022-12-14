
library(xlsx)




library(ggplot2)  # The grammar of graphics package
library(maps)     # Provides latitude and longitude data for various maps
library(dplyr)    # To assist with cleaning and organizing data


## Load in data
stateObservations <- read.xlsx("./year_state_observations.xlsx", 1)
MainStates <- map_data("state")

## Merge the data
MergedStates <- inner_join(MainStates, stateObservations, by = "region")

## Plot 
g <- ggplot()
g <- g + geom_polygon( data=MergedStates, 
                       aes(x=long, y=lat, group=group, fill = population), 
                       color="black", size = 0.2) + 
  
  scale_fill_continuous(name="UFO sightings", low = "lightblue", 
                        high = "darkblue",  
                        na.value = "red") +
  
  labs(title="Observations of UFO's by states")
g