################################
#The amazing data visualization
#project, created by group 13
###############################
#Source: https://data.world/aarranzlopez/ufo-sights-2016-us-and-canada

library(shiny)
library(xlsx)
library(tidyverse)
library(sf)
library(mapview)
library(viridis)
library(RColorBrewer)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(gganimate)
#Wordcloud packages



ourdata <- read.xlsx("./UFOs_coord-1.xlsx", 1)

#summary(ourdata)
#ourdata %>% 
#  glimpse()


#ourdata <- ourdata  %>% 
#  filter(State == "CA")

#plot(ourdata$lat, ourdata$lng, xlab = "Latitude", ylab = "Longitude")


##This is to visualize the different locations on a map. 


theme_set(theme_bw())

mapview(ourdata, xcol = "lng", ycol = "lat", crs = 4269, grid = FALSE)

colfunc <- colorRampPalette(c("white", "red"))

counts_shape <- table(ourdata$Shape)
barplot_shapes <- barplot(counts_shape, main="Shape distribution",
                          xlab="Shapes observed", col=colfunc(30) , beside=False)


counts_state <- table(ourdata$State)
barplot_shapes <- barplot(counts_state, main="State distribution",
                          xlab="Observations in states", col=colfunc(60) , beside=False)

#sightings_frequency <- ggplot()
p <- ggplot(
  ourdata,
  aes(Day, Temp, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")



ui <- dashboardPage(
  dashboardHeader(title = "UFO Sightings"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bar Plots", tabName = "bar_plots"),
      menuItem("Map Plots", tabName = "map_plots"),
      menuItem("Interactive Plots", tabName = "interactive_plots"),
      menuItem("Data", tabName = "data"),
      menuItem("Animated Plots", tabName = "animated_plots")
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "bar_plots",
        box(plotOutput("bar_plot"), width = 8)
              ),
      tabItem(
        "map_plots",
              box(leafletOutput("map_view"), width = 8)
        ),
      tabItem(
        "interactive_plots",
        h1("Interactive Plots")
      ),
      tabItem(
        "data",
        h1("Data"),
        dataTableOutput("ufotable")
        
      ),
      tabItem(
        "animated_plots",
        h1("Sighting Frequency")
      )
    )
    #box(plotOutput("bar_plot"), width = 8),
    #box(leafletOutput("map_view"), width = 8)
  )
)


server <-function(input, output){
  output$bar_plot <- renderPlot({
    barplot(counts_state, main="State distribution",
            xlab="Observations in states", col=colfunc(60) , beside=False)
  })
  output$map_view <- renderLeaflet({
    mapview(ourdata, xcol = "lng", ycol = "lat", crs = 4269, grid = FALSE)@map
    
  })
  
  output$ufotable <- renderDataTable(ourdata)
}



shinyApp(ui, server) 

