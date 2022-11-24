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
#Wordcloud packages



ourdata <- read.xlsx("./UFOs_coord-1.xlsx", 1)

#summary(ourdata)
#ourdata %>% 
#  glimpse()


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


####Wordcloud

text <- readLines("./words.txt")

myCorpus = Corpus(VectorSource(text))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,
                  c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))

myDTM = TermDocumentMatrix(myCorpus,
                           control = list(minWordLength = 1))

m = as.matrix(myDTM)

#sort(rowSums(m), decreasing = TRUE)

wordcloud(sort(rowSums(m), decreasing = TRUE))
###




ui <- dashboardPage(
  dashboardHeader(title = "UFO Sightings"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bar-plots", tabName = "bar_plots"),
      menuItem("Map-plots", tabName = "map_plots"),
      menuItem("Interactive-plots", tabName = "interactive_plots"),
      menuItem("Data", tabName = "data"), 
      menuItem("WORDCLOUD", tabName = "wordcloud"),
      menuItem("FAQ", tabName = "faq"), 
      
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
        "wordcloud",
        h1("Wordcloud"),
        box()
      ),
      ), 
      tabItem(
        "faq",
        h1("FAQ"),
        p("Here is a list of a FAQ"), 
        
        h1("How often do people in the US and Canada see UFO’s?"),
        p("answers"), 
        
        h1("How do we curate our chosen data, in a way that makes
           it easier to understand while highlighting useful information?"), 
        p("answer"), 
        
        h1("What are the most typical seen shapes of UFO’s, and how can it 
        best be visualized? "),
        p(""),
        
        h1("How do we VIsualize map coordinates in a datavisualization"), 
        p(""), 
        
        h1("How many UFO’s spotted in a given state/area?"),
        p(""),
        
        h1("Are there certain keywords which are more common in the sightings summary"),
        p(""),
        
        h1("Are there any anomalies in the data set?"), 
        p(""), 
        
        h1("Which state(s) are there observed the most sightings?"),
        p(""),
        
        h1(""),
        p(""),
        
        
        
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

