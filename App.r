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
library(tm)
library(wordcloud)
library(memoise)
library(ggplot2)
library(gganimate)
library(dplyr)
library(scales)



ourdata <- read.xlsx("./UFOs_coord-1.xlsx", 1)


theme_set(theme_bw())

#mapview(ourdata, xcol = "lng", ycol = "lat", crs = 4269, grid = FALSE)

colfunc <- colorRampPalette(c("white", "green"))

counts_shape <- table(ourdata$Shape)
barplot_shapes <- barplot(counts_shape, main="Shape distribution",
                          xlab="Shapes observed", col=colfunc(30) , beside=False)

## data for choroplethmap
stateObservations <- read.xlsx("./year_state_observations.xlsx", 1)
MainStates <- map_data("state")
## Merge the choropleth data
MergedStates <- inner_join(MainStates, stateObservations, by = "region")



counts_state <- table(ourdata$State)
counts_date <- table(ourdata$Date...Time)
barplot_shapes <- barplot(counts_state, main="State distribution",
                          xlab="Observations in states", col=colfunc(60) , beside=False)

#####
#Date State Observations
####

date_state_set <- read.xlsx("./Date_State_Observation.xlsx", 1)


date_state_set <- date_state_set  %>% 
  filter(State %in% c(" CO", " VA", " CA", " WA"))


animated_data_full_date <- data.frame (
  dates = c(as.Date(date_state_set$Date, format = "%m-%d-%y")),
  states = c(date_state_set$State), 
  observations = c(date_state_set$Observations)
)

animated_data_month <- data.frame (
  month = c(as.integer(date_state_set$Month)),
  states = c(date_state_set$State), 
  observations = c(date_state_set$Observations)
)

#animated_data_month_sorted <- animated_data_month[order(animated_data_month$month, decreasing=TRUE),]

animated_plot <- ggplot(
  animated_data_full_date,
  aes(dates, observations, group = states, color = factor(states))
) +
  geom_line() +  
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Date", y = "Observations in state") +
  theme(legend.position = "top") + 
  xlim(as.Date(c("1-1-16", "12-12-16"), format="%m-%d-%y"))

#Does not work
#graph_animation = animated_plot +
#  transition_time(c(as.Date(date_state_set$Date, format = "%m-%d-%y")))
#


##Circle plot here##
####
 
circle_data <- read.xlsx("./shape_distribution.xlsx", 1)

data_circle <- data.frame(
  id=circle_data$id,
  individual=c(circle_data$individual),
  value=c(circle_data$value)
)

label_data <- data_circle
# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)


# Start the plot
circle_plot <- ggplot(data_circle, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("green", 0.9), colour="black") +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-120, 700) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  scale_radius() +
  
  scale_color_gradient(low="white", high="green") + 
  
  labs(title="Shape Circle Distribution") + 
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1.9, size=3.5, angle= label_data$angle, inherit.aes = FALSE ) 

circle_plot

####
#Wordcloud
####

getWordCloud <- memoise(function() {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  
  text <- readLines("./words.txt")

  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
  
   
})



ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "UFO Sightings"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bar-plots", tabName = "bar_plots"),
      menuItem("Map-plots", tabName = "map_plots"),
      menuItem("Common-Descripters", tabName = "wordcloud"),
      menuItem("Animated Plots", tabName = "animated_plots"),      
      menuItem("Report", tabName = "Report"), 
      menuItem("Data", tabName = "data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "bar_plots",
        box(plotOutput("bar_plot"), width = 16,
        box(plotOutput("bar_shape_plot")), 
        box(plotOutput("circle_plot")), 
        
      ),
      tabItem(
        "map_plots",
        box(leafletOutput("map_view"), width = 8),
        box(plotOutput('choropleth'), width = 8)
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
        "wordcloud",
        h1("Wordcloud"),
        box(
          #titlePanel("Word Cloud"),
          
          sidebarLayout(
            # Sidebar with a slider and selection inputs
            sidebarPanel(
              #actionButton("update", "Change"),
              hr(),
              sliderInput("freq",
                          "Minimum Frequency:",
                          min = 1,  max = 1000, value = 20),
              sliderInput("max",
                          "Maximum Number of Words:",
                          min = 1,  max = 25,  value = 15)
            ),
            
            # Show Word Cloud
            mainPanel(
              plotOutput("plot_map")
            )
          ), width = 12, height = 12)
      ),
      tabItem(
        "Report",
        
        tags$a(href="https://github.com/JoachimBaumann/DataVis-2022", "Link To Report", download=NA, target="_blank"),
      
        p(""),
        
        shiny::actionButton(inputId='ab1', label="Download Report", 
                            icon = icon("th"), 
                            onclick ="window.open('https://github.com/JoachimBaumann/DataVis-2022/raw/DashboardStructure/pdf.pdf', '_blank')")
        
      ),
      tabItem(
        "animated_plots",
        h1("Sighting Frequency"), 
        box(plotOutput("date_state_observation_plot")),
        p("Animation"),
        img(src="animated_plot3.gif", align = "left",height='400px',width='600px')
      )
    )
    #box(plotOutput("bar_plot"), width = 8),
    #box(leafletOutput("map_view"), width = 8)
  )
)


server <-function(input, output, session){
  output$bar_plot <- renderPlot({
    barplot(counts_state, main="State distribution",
            xlab="Observations in states", col=colfunc(80) , beside=TRUE, 
            cex.axis=1.1, cex.names=0.9, las = 2) 
  })
  
  
  output$bar_shape_plot <- renderPlot({
    barplot(counts_shape, main="Shape distribution",
             col=colfunc(60) , beside=FALSE, cex.axis=1.9, cex.names=0.9, las = 2)
  })
  
  output$circle_plot <- renderPlot({circle_plot})
  
  
  output$map_view <- renderLeaflet({
    mapview(ourdata, xcol = "lng", ycol = "lat", crs = 4269, grid = FALSE)@map
    
  })
  output$choropleth <- renderPlot({
    
    
    ggplot() +
    geom_polygon( data=MergedStates, 
                           aes(x=long, y=lat, group=group, fill = population), 
                           color="black", size = 0.2) + 
      
      scale_fill_continuous(name="UFO sightings", low = "lightblue", 
                            high = "darkblue",  
                            na.value = "red") +
      
      labs(title="Observations of UFO's by states")
  })
  
  output$date_range <- renderPrint({ 
    
    #Sort by date here 
    input$dates 
    
    })
  
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getWordCloud()
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot_map <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  #date state observations
  
  
  output$date_state_observation_plot <- renderPlot(ggplot(
    animated_data_month,
    aes(x=factor(month, level=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), y=observations, group = states, color = factor(states))
  ) +
    geom_line() +  
    geom_point() +
    scale_color_viridis_d() +
    scale_x_discrete() +
    labs(x = "Month", y = "Observations in state") +
    theme(legend.position = "top"))
    #+ 
    #xlim(as.Date(c("1-1-16", "12-12-16"), format="%m-%d-%y")))
    #xlim(c(0,13)))
  
  
  
  
  #data table 
  
  output$ufotable <- renderDataTable(ourdata)
}



shinyApp(ui, server) 

