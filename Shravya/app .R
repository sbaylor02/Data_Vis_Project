####SHRAVYA
library(shiny)
library(rgdal)
library(leaflet)



s1 <- readOGR(dsn = '2010_CensusData', "2010_CensusData", stringsAsFactors = FALSE)
pal <- colorNumeric(palette = "viridis", 
                    domain = s1$SE_T002_01)


# Define UI for application that draws a histogram
ui <- fluidPage(  
  titlePanel("Choose a race and click on a census district to see the race's population within the district"),
  column(8,leafletOutput("map", height="800px")))

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Leaflet map: population by district by race 
  output$map <- renderLeaflet({
    m <- leaflet()  %>%
      addTiles()  %>%addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), group= "South Bend Boundaries")%>%addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), popup = ~SE_T054_01, group= "Total Population")%>%addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), popup = ~SE_T054_02, group= "White/Caucasian") %>% addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), popup = ~SE_T054_03, group= "Black/African American")%>%addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), popup = ~SE_T054_04, group= "American Indian/Alaska Native") %>%addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), popup = ~SE_T054_05, group= "Asian")%>%addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), popup = ~SE_T054_06, group= "Native Hawaiian/Pacific Islander")%>%
      addPolygons(data = s1, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal(SE_T002_01), popup = ~SE_T054_07, group= "Other")%>%addLayersControl(
        overlayGroups = c("South Bend Boundaries"),
        baseGroups  = c("Total Population", "White/Caucasian", "Black/African American", "American Indian/Alaska Native", "Asian", "Native Hawaiian/Pacific Islander", "Other"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
}    

# Run the application 
shinyApp(ui = ui, server = server)

