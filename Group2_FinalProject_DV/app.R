#Group 2 Shiny App
#Catherine, Pavel, Rose, Sarah, Shravya
#East
#Final Project

#load the necessary packages
library(shiny)
library(plyr)
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

##########Catherine
#load Abandoned_Property_Parcels shapefile
abandPropParc <- readOGR(dsn = "Abandoned_Property_Parcels", 
                         layer = "Abandoned_Property_Parcels", 
                         stringsAsFactors = FALSE)

#extract data into separate data frame
abandPropParc_data <- abandPropParc@data

#change variable type of Date_of_Ou to date
abandPropParc_data$Date_of_Ou <- as.Date(abandPropParc_data$Date_of_Ou)

#add variable to show year of Date_of_OU
abandPropParc_data$Year_of_Ou <- year(abandPropParc_data$Date_of_Ou)




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Data Builds a Stronger South Bend"),
  # Show a plot of the generated distribution
  tabsetPanel(type = "tabs",
              id = "myTabs",
    #####Rose
    tabPanel("Age/Gender Demographics",
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
    #####Shravya
    tabPanel("Racial Demographics",
             fluid = TRUE,
             value = "tab2",
             titlePanel("Choose a race and click on a census district to see the race's population within the district"),
             column(8,leafletOutput("map_shravya", height="800px"))),
    #####Pavel
    tabPanel("Poles Ownership Distribution",
             fluid = TRUE,
             id = "tab3",
             selectInput(inputId = "owner",
                         label = "Ownership", 
                         choices = unique(street_lights$Ownership)),
             leafletOutput("mymap",height = 1000)),
    #####Sarah
    tabPanel("Abandoned Properties by District", 
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
    #####Catherine
    tabPanel("Abandoned Properties Figures",
             fluid = TRUE,
             value = "tab5",
             # Application title
             titlePanel("South Bend Abandoned Properties Parcels"),
             # Sidebar with various inputs to control both the line and bar chart outputs
             sidebarLayout(
               sidebarPanel(
                 # add help text to guide the user to make selections
                 helpText("Select desired details with which to customize the charts by 
                          using the various inputs below."),
                 # add help text to guide the user to make selections
                 helpText("NA selections denote missing values."),
                 # add multi-check box input selection for the type of outcome(s)
                 checkboxGroupInput(inputId = "outcome",
                                    label = "Abandoned Property Parcel Outcome:",
                                    choices = c("Demolished", "Deconstructed", "Repaired",
                                                "Repaired & Occupied", "Occupied & Not Repaired", 
                                                "NA"),
                                    selected = c("Demolished", "Deconstructed", "Repaired",
                                                 "Repaired & Occupied", "Occupied & Not Repaired", 
                                                 "NA")),
                 # add multi-check box input selection for the city council district(s) to view
                 checkboxGroupInput(inputId = "councilDistrict",
                                    label = "City Council District:",
                                    choices = c("1", "2", "3", "4", "5", "6", "NA"),
                                    selected = c("1", "2", "3", "4", "5", "6", "NA")),
                 # add slider input to select which year range of data to show
                 # date range of data is 3/28/13 to 3/23/18
                 sliderInput(inputId = "years",
                             "Years of Data to Display:",
                             min = 2013,
                             max = 2018,
                             value = c(2013, 2018),
                             sep = ""),
                 # add help text to define available date range
                 helpText("Property outcome dates range from March 2013 through March 2018.")
                 ),
               # Show the generated line and bar charts
               mainPanel(
                 # line chart of outcome figures over time
                 plotOutput("outcomePlot"),
                 # bar chart of code enforcment details
                 plotOutput("codeEnforPlot")
               )
             ))
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
  # generate inputs based on input$outcome, input$councilDistrict, & input$dateRange
  # from ui.R
  
  APP_data <- reactive({
    abandPropParc_data[(abandPropParc_data$Outcome_St %in% input$outcome) & 
                         (abandPropParc_data$Council_Di %in% input$councilDistrict) &
                         (abandPropParc_data$Year_of_Ou >= input$years[1] & 
                            abandPropParc_data$Year_of_Ou <= input$years[2]),]
  })
  
  # generate frequency table for line chart
  APP_data_freq <- reactive({
    plyr::count(APP_data(), c("Outcome_St", "Year_of_Ou"))
  })
  
  
  # draw line chart displaying outcome results over time
  output$outcomePlot <- renderPlot({
    ggplot(data = APP_data_freq(),
           aes(x = APP_data_freq()$Year_of_Ou, 
               y = APP_data_freq()$freq, 
               group = APP_data_freq()$Outcome_St, 
               color = APP_data_freq()$Outcome_St)) +
      geom_line() +
      geom_point() +
      labs(title = "Parcel Outcomes Over Time", x = "Year", y = "Count of Parcels") +
      scale_color_discrete("Parcel Outcome") +
      theme_minimal()
  })
  
  # draw bar chart displaying 
  output$codeEnforPlot <- renderPlot({
    ggplot(data = APP_data(), 
           aes(x = APP_data()$Code_Enfor)) +
      geom_bar() +
      labs(title = "Parcel Code Enforcement Distribution", 
           x = "Code Enforcement Categories",
           y = "Count of Parcels") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 40,
                                       hjust = 1,
                                       vjust = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

