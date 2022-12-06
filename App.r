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

colfunc <- colorRampPalette(c("white", "red"))

counts_shape <- table(ourdata$Shape)
barplot_shapes <- barplot(counts_shape, main="Shape distribution",
                          xlab="Shapes observed", col=colfunc(30) , beside=False)


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
  month = c(date_state_set$Month),
  states = c(date_state_set$State), 
  observations = c(date_state_set$Observations)
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

#Does not work
#graph_animation = animated_plot +
#  transition_time(c(as.Date(date_state_set$Date, format = "%m-%d-%y")))
#


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
      menuItem("FAQ", tabName = "faq"), 
      menuItem("Data", tabName = "data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "bar_plots",
        box(plotOutput("bar_plot"), width = 8),
        box(plotOutput("bar_shape_plot"))
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
        "faq",
        tags$b("FAQ"),
        p("Here is a list of a FAQ"), 
        
        tags$b("How often do people in the US and Canada see UFO’s?"),
        p("placeholder answer"), 
        
        tags$b("How do we curate our chosen data, in a way that makes
           it easier to understand while highlighting useful information?"), 
        p("placeholder answer"), 
        
        tags$b("What are the most typical seen shapes of UFO’s, and how can it 
        best be visualized? "),
        p("placeholder answer"),
        
        tags$b("How do we VIsualize map coordinates in a datavisualization"), 
        p("placeholder answer"), 
        
        tags$b("How many UFO’s spotted in a given state/area?"),
        p("placeholder answer"),
        
        tags$b("Are there certain keywords which are more common in the sightings summary"),
        p("placeholder answer"),
        
        tags$b("Are there any anomalies in the data set?"), 
        p("placeholder answer"), 
        
        tags$b("Which state(s) are there observed the most sightings?"),
        p("placeholder answer"),
        
        tags$b(""),
        p("")
        
      ),
      tabItem(
        "animated_plots",
        h1("Sighting Frequency"), 
        box(plotOutput("date_state_observation_plot")),
        p("Animation"),
        img(src="animated_plot.gif", align = "left",height='400px',width='600px')
      )
    )
    #box(plotOutput("bar_plot"), width = 8),
    #box(leafletOutput("map_view"), width = 8)
  )
)


server <-function(input, output, session){
  output$bar_plot <- renderPlot({
    barplot(counts_state, main="State distribution",
            xlab="Observations in states", col=colfunc(60) , beside=False)
  })
  output$map_view <- renderLeaflet({
    mapview(ourdata, xcol = "lng", ycol = "lat", crs = 4269, grid = FALSE)@map
    
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
    aes(month, observations, group = states, color = factor(states))
  ) +
    geom_line() +  
    geom_point() +
    scale_color_viridis_d() +
    labs(x = "Month", y = "Observations in state") +
    theme(legend.position = "top")) #+ 
    #xlim(as.Date(c("1-1-16", "12-12-16"), format="%m-%d-%y")))
    #xlim(c(0,13)))
  
  
  
  
  #data table 
  
  output$ufotable <- renderDataTable(ourdata)
}



shinyApp(ui, server) 

