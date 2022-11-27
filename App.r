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
library(dplyr)
library(gganimate)
#Wordcloud packages



ourdata <- read.xlsx("./UFOs_coord-1.xlsx", 1)

#load words for wordmap 
#text <- readLines("./words.txt")


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


####Wordcloud

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
                 actionButton("update", "Change"),
                 hr(),
                 sliderInput("freq",
                             "Minimum Frequency:",
                             min = 1,  max = 500, value = 50),
                 sliderInput("max",
                             "Maximum Number of Words:",
                             min = 1,  max = 1000,  value = 500)
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
        h1("Sighting Frequency")
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
  output$bar_shape_plot <- renderPlot({
    barplot(counts_shape, main="Shape distribution",
            xlab="Shapes observed", col=colfunc(30) , beside=False)
    
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
  
  
  output$ufotable <- renderDataTable(ourdata)
}



shinyApp(ui, server) 
