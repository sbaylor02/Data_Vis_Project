#Set up shiny app for final project

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
districts <- readOGR(dsn = "City_Council_Districts", 
                     layer = "City_Council_Districts",
                     stringsAsFactors = FALSE)

abandoned <- readOGR(dsn = "Abandoned_Property_Parcels",
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
   titlePanel("Abandoned Properties by Council District"),
   
   # Group Checkbox
   sidebarLayout(
      sidebarPanel(
         #create a checkbox to select city council districts
         checkboxGroupInput(inputId = "district",
                            label = "Choose a City Council District",
                            choices = unique(districts$Num),
                            selected = unique(districts$Num)),
         #create a checkbox to select the outcome of the abandoned property
         checkboxGroupInput(inputId = "outcome",
                            label = "Choose an Outcome",
                            choices = unique(abandoned$Outcome_St),
                            selected = unique(abandoned$Outcome_St)),
         #create a checkbox for the year of the outcome for the abandoned property
         checkboxGroupInput(inputId = "year",
                            label = "Choose a Year",
                            choices = sort(unique(abandoned_center$Year_of_Ou)),
                            selected = unique(abandoned_center$Year_of_Ou))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("district_map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  pal <- colorFactor(palette = 'Set1', domain = abandoned_center$Outcome_St)
  districts$popup_dist <- paste("Council District: ", districts$Dist, "<br>",
                      "Number: ", districts$Num, "<br>",
                      "Council Member: ", districts$Council_Me)
   
   output$district_map <- renderLeaflet({
     #create a leaflet map
     leaflet(data = districts[districts$Num %in% input$district,]) %>%
       addTiles() %>%
       addPolygons(color = "green", 
                   weight = 1,
                   smoothFactor = 0.5,
                   opacity = 1.0,
                   fillOpacity = 0.5,
                   fillColor = ~colorFactor(palette = "YlGn", domain = Dist)(Dist),
                   popup = ~popup_dist) %>%
       addCircleMarkers(data = abandoned_center[(abandoned_center$Council_Di %in% input$district) & 
                                                  (abandoned_center$Outcome_St %in% input$outcome) &
                                                  (abandoned_center$Year_of_Ou %in% input$year),],
                        radius = 2,
                        popup = ~Suffix,
                        options = list(zindex = 10),
                        color = ~pal(Outcome_St))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

