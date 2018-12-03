#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries, packages and data
library(devtools)
devtools::install_github('dkahle/ggmap')
library(ggmap)
register_google(key = "AIzaSyArtVYaFH3qvcVP2ufW5_LMMANBwZK2vS4")
library(rgdal)
library(tidyr)



s1 <- readOGR('/Users/shravyaguda/Documents/Notre Dame MSDS/Fall 2018/Data Visualization/Data-Viz-2018-Fall-master 2/FinalProject/2010_CensusData', stringsAsFactors = FALSE)
library(tidyverse)

library(tidycensus)

library(rgeos)
library(leaflet)
library(rgdal)
#census_api_key('1bed28e6e69509571c7648b6374a52a69c99111f', install=TRUE)

library(shiny)
library(leaflet)

s1$popup <- paste(s1$NAME)


pal <- colorNumeric(palette = "viridis", 
                          domain = s1$SE_T002_01)

server <- function(input, output) {
  
  # build data with 2 places
  data=data.frame(s1, id=s1$NAME)
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map: black/african american alone by population density???
  output$map <- renderLeaflet({
    m <- leaflet()  %>%
      addTiles()  %>%
      addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), popup = ~SE_T054_03) 
    
    m %>% addProviderTiles(providers$MtbMap) %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions()) 
    
  })
  # 
  # # store the click
  # observeEvent(input$map_marker_click,{
  #   data_of_click$clickedMarker <- input$map_marker_click
  # })
  # 
  # # Make a barplot  depending of the selected point
  # output$bar <- renderPlot(
  #   pop_agg <- aggregate()
  # )
  }    
 
#titlePanel(title, windowTitle = title)

ui <- fluidPage(
  titlePanel("African American Population by Population Density"),
  br(),
  column(8,leafletOutput("map", height="600px")),
  column(4,br(),br(),br(),br(),plotOutput("bar", height="300px")),
  br()
)

shinyApp(ui = ui, server = server)

