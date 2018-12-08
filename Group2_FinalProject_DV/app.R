#Group 2 Shiny App
#Catherine, Pavel, Rose, Sarah, Shravya
#East
#Final Project

#load the necessary packages
library(shiny)
library(tidyverse)
library(tidycensus)
library(tidyr)
library(leaflet)
library(rgdal)
library(DT)
library(rgeos)
library(sp)
library(lubridate)
library(ggplot2)
library(ggmap)
library(reshape2)
register_google(key = "AIzaSyArtVYaFH3qvcVP2ufW5_LMMANBwZK2vS4")

##########Rose
#load census
s1 <- readOGR("2010_CensusData", 
              "2010_CensusData", 
              stringsAsFactors = FALSE)

#preprocess age data
population_sub <- as.data.frame(s1) %>% 
  select(NAME, SE_T008_02, SE_T008_03, SE_T008_04, SE_T008_05, SE_T008_06,
         SE_T008_07, SE_T008_08, SE_T008_09, SE_T008_10, SE_T008_11, SE_T008_12)
colnames(population_sub) <- c("NAME", "<5", "5-9", "10-14", "15-17", "18-24", 
                              "25-34", "35-44", "45-54", "55-64", "75-84", ">85")
population_sub_long <- melt(population_sub, 
                            measure.vars = c("<5", "5-9", "10-14", "15-17", 
                                             "18-24", "25-34", "35-44", "45-54", 
                                             "55-64", "75-84", ">85"))
population_sub_long$value <- as.integer(population_sub_long$value)

#preprocess gender data
gender_sub <- as.data.frame(s1) %>% 
  select(NAME,SE_T003_01, SE_T003_02, SE_T003_00)
colnames(gender_sub) <- c("NAME", "Male", "Female", "Total")
gender_sub$Male <- as.integer(gender_sub$Male)
gender_sub$Female <- as.integer(gender_sub$Female)
gender_sub$Total <- as.integer(gender_sub$Total)
gender_sub_long <- melt(gender_sub, id.vars=c("NAME", "Total"))

#create map popup
s1$popup <- paste("<b>",s1$NAMELSAD,"</b><br>",
                  "Population: ",s1$SE_T003_00,sep ="")

##########Shravya
s2 <- readOGR(dsn = '2010_CensusData', "2010_CensusData", stringsAsFactors = FALSE)


##########Sarah
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

##########Pavel
street_lights <- read.csv("Street_Lights.csv", 
                          stringsAsFactors = F)
street_lights[street_lights$Pole_Type %in% c(""," "),]$Pole_Type <- "Unknown"
street_lights[street_lights$Service %in% c(""," "),]$Service <- "Unknown"
street_lights$Inspect_Date2 <- as.Date(street_lights$Inspect_Date)




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Data Builds a Stronger South Bend"),
  # Show a plot of the generated distribution
  tabsetPanel(type = "tabs",
              id = "myTabs",
    tabPanel("rose",
             fluid = TRUE,
             value = "tab1",
             headerPanel("Select a Census Tract"),
             br(),
             column(7,
                    leafletOutput("map_rose", height="500px")),
             column(5, 
                    plotOutput("barpop", height="300px"),
                    br(), 
                    plotOutput("bargen", height="200px")),
             br()),
    tabPanel("shravya",
             fluid = TRUE,
             value = "tab2",
             titlePanel("Choose a race and click on a census district to see the race's population within the district"),
             column(8,leafletOutput("map_shravya", height="800px"))),
    tabPanel("pavel",
             fluid = TRUE,
             id = "tab3",
             selectInput(inputId = "owner",
                         label = "Ownership", 
                         choices = unique(street_lights$Ownership)),
             leafletOutput("mymap",height = 1000)),
    tabPanel("sarah", 
             fluid = TRUE, 
             value = "tab4",
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
                                    selected = unique(abandoned_center$Year_of_Ou))),
               # Show a plot of the generated distribution
               mainPanel(leafletOutput("district_map")))),
    tabPanel("catherine",
             fluid = TRUE,
             value = "tab5")
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##########Rose
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map 
  output$map_rose <- renderLeaflet({
    leaflet()  %>%
      addTiles()  %>%
      addPolygons(data = s1, 
                  layer=~NAME, 
                  stroke = TRUE, 
                  fillOpacity = .35,  
                  popup = ~popup)
  })
  
  # store the click
  observeEvent(input$map_rose_shape_click, {
    p<- input$map_rose_shape_click 
    print((p)) 
  }
  ) 
  
  # Make a age barplot  depending of the selected point
  
  output$barpop <- renderPlot({
    
    population_sub_long2 <- population_sub_long %>% 
      filter (NAME==ifelse(is.null(input$map_rose_shape_click$id), 
                           NAME,
                           input$map_rose_shape_click$id))
    
    # filter (NAME==ifelse(is.null(input$map_rose_shape_click$id), NAME,input$map_rose_shape_click$id))
    
    # draw the histogram with the specified number of bins
    ggplot(population_sub_long2, aes(x=variable, y=value))+
      geom_bar(stat = "identity", position = "stack", fill="royalblue2") +
      scale_x_discrete("Age group")+ theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1), 
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))+ 
      #\ylim(0, 1400) +
      labs (y= "Population", title= "Population Age Distribution", 
            subtitle= ifelse(is.null(input$map_rose_shape_click$id),
                             "Plot of Census Tract 15", 
                             (paste("Plot of Census Tract",
                                    input$map_rose_shape_click$id))))})
  
  # Make a gender barplot  depending of the selected point
  
  output$bargen <- renderPlot({
    
    gender_sub_long2 <- gender_sub_long %>% 
      filter (NAME==ifelse(is.null(input$map_rose_shape_click$id), 
                           NAME,
                           input$map_rose_shape_click$id))
    
    ggplot(gender_sub_long2, 
           aes(x=NAME, y=(value/Total), fill=variable), 
           label=value) + 
      geom_bar(position = "fill",
               stat = "identity", 
               fill=c("salmon", "royalblue3"))  +
      theme_void() +
      theme(legend.position="none", 
            plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5))+ 
      geom_text(aes(label = paste(variable,"", round(value/Total, 4)*100,"%")), 
                position = position_stack(vjust = .5))+ 
      coord_flip()+ 
      labs ( title= "Population Gender Distribution", 
             subtitle= ifelse(is.null(input$map_rose_shape_click$id),
                              "Plot of Census Tract 15", 
                              (paste("Plot of Census Tract",
                                     input$map_rose_shape_click$id))))
    
  })
  
  ##########Shravya
  # Leaflet map: population by district by race 
  pal1 <- colorNumeric(palette = "viridis", 
                       domain = s1$SE_T002_01)
  
  output$map_shravya <- renderLeaflet({
    m <- leaflet()  %>%
      addTiles()  %>%addPolygons(data = s2, 
                                 stroke = TRUE, 
                                 smoothFactor = 0.2, 
                                 fillOpacity = .35, 
                                 color = ~pal1(SE_T002_01), 
                                 group= "South Bend Boundaries") %>%
      addPolygons(data = s2, 
                  stroke = TRUE, 
                  smoothFactor = 0.2, 
                  fillOpacity = .35, 
                  color = ~pal1(SE_T002_01), 
                  popup = ~SE_T054_01, 
                  group= "Total Population") %>% 
      addPolygons(data = s2, 
                  stroke = TRUE, 
                  smoothFactor = 0.2, 
                  fillOpacity = .35, 
                  color = ~pal1(SE_T002_01), 
                  popup = ~SE_T054_02, 
                  group= "White/Caucasian") %>% 
      addPolygons(data = s2, 
                  stroke = TRUE, 
                  smoothFactor = 0.2, 
                  fillOpacity = .35, 
                  color = ~pal1(SE_T002_01), 
                  popup = ~SE_T054_03, 
                  group= "Black/African American") %>% 
      addPolygons(data = s2, 
                  stroke = TRUE, 
                  smoothFactor = 0.2, 
                  fillOpacity = .35, 
                  color = ~pal1(SE_T002_01), 
                  popup = ~SE_T054_04, 
                  group= "American Indian/Alaska Native") %>%
      addPolygons(data = s2, 
                  stroke = TRUE, 
                  smoothFactor = 0.2, 
                  fillOpacity = .35, 
                  color = ~pal1(SE_T002_01), 
                  popup = ~SE_T054_05, 
                  group= "Asian") %>%
      addPolygons(data = s2, 
                  stroke = TRUE, 
                  smoothFactor = 0.2, 
                  fillOpacity = .35, 
                  color = ~pal1(SE_T002_01), 
                  popup = ~SE_T054_06, 
                  group= "Native Hawaiian/Pacific Islander") %>%
      addPolygons(data = s2, stroke = TRUE, smoothFactor = 0.2, fillOpacity = .35, color = ~pal1(SE_T002_01), popup = ~SE_T054_07, group= "Other")%>%addLayersControl(
        overlayGroups = c("South Bend Boundaries"),
        baseGroups  = c("Total Population", "White/Caucasian", 
                        "Black/African American", "American Indian/Alaska Native", 
                        "Asian", "Native Hawaiian/Pacific Islander", "Other"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  ##########Pavel
  data <- reactive({
    x <- street_lights[street_lights$Ownership == input$owner,]
    return(x)
  })
  
  output$mymap <- renderLeaflet({
    street_lights <- data()
    
    m <- leaflet(data = street_lights) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Lon,
                       lat = ~Lat, radius = 2)
    m
  })
  
  ##########Sarah
  pal2 <- colorFactor(palette = 'Set1', domain = abandoned_center$Outcome_St)
  districts$popup_dist <- paste("Council District: ", districts$Dist, "<br>",
                                "Number: ", districts$Num, "<br>",
                                "Council Member: ", districts$Council_Me)
  

  output$district_map <- renderLeaflet({
    #create a leaflet map for the given districts
    leaflet(data = districts[districts$Num %in% input$district,]) %>%
      #add the map
      addTiles() %>%
      #add the shapes of the districts to the map
      addPolygons(color = "green", 
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  fillColor = ~colorFactor(palette = "YlGn", domain = Dist)(Dist),
                  popup = ~popup_dist) %>%
      #add the chosen abandon properties to the map
      addCircleMarkers(data = abandoned_center[(abandoned_center$Council_Di %in% input$district) & 
                                                 (abandoned_center$Outcome_St %in% input$outcome) &
                                                 (abandoned_center$Year_of_Ou %in% input$year),],
                       radius = 2,
                       popup = ~Suffix,
                       options = list(zindex = 10),
                       color = ~pal2(Outcome_St))
  })

  
  ##########Catherine
}

# Run the application 
shinyApp(ui = ui, server = server)

