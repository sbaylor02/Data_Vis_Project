#Group 2 Shiny App
#Catherine, Pavel, Rose, Sarah, Shravya
#East
#Final Project

#set working directory
setwd("~/Desktop/Final_Project_Practice/")

#load the necessary packages
library(shiny)
library(tidyverse)
library(leaflet)
library(rgdal)
library(DT)
library(rgeos)
library(sp)
library(lubridate)

#read in the data
districts <- readOGR(dsn = "~/Documents/GitHub/Data_Vis_Project/Group2_FinalProject_DV/City_Council_Districts/", 
                     layer = "City_Council_Districts",
                     stringsAsFactors = FALSE)

abandoned <- readOGR(dsn = "~/Documents/GitHub/Data_Vis_Project/Group2_FinalProject_DV/Abandoned_Property_Parcels/",
                     layer = "Abandoned_Property_Parcels",
                     stringsAsFactors = FALSE)

#find the center of each abandoned property to create points for the map
abandoned_center <- SpatialPointsDataFrame(gCentroid(abandoned, 
                                                     byid = TRUE),
                                           abandoned@data,
                                           match.ID = FALSE)

#add the year of outcome as a column in the abandoned data set
abandoned_center@data$Year_of_Ou <- as.ordered(year(abandoned_center@data$Date_of_Ou))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

