#Group 2 Shiny App
#Catherine, Pavel, Rose, Sarah, Shravya
#East
#Final Project

library(shiny)
library(leaflet)
library(tidyverse)
library(tidycensus)
library(rgeos)
library(rgdal)
library(ggmap)
library(reshape2)
register_google(key = "AIzaSyArtVYaFH3qvcVP2ufW5_LMMANBwZK2vS4")

#load census
s1 <- readOGR(".", "2010_CensusData", stringsAsFactors = FALSE)

#preprocess age data
population_sub <- as.data.frame(s1) %>% select(NAME,SE_T008_02, SE_T008_03, SE_T008_04, SE_T008_05,SE_T008_06,SE_T008_07,SE_T008_08,SE_T008_09,SE_T008_10,SE_T008_11,SE_T008_12)
colnames(population_sub) <- c("NAME", "<5", "5-9", "10-14", "15-17", "18-24", "25-34", "35-44", "45-54", "55-64", "75-84", ">85")
population_sub_long <- melt(population_sub, measure.vars = c("<5", "5-9", "10-14", "15-17", "18-24", "25-34", "35-44", "45-54", "55-64", "75-84", ">85"))
population_sub_long$value <- as.integer(population_sub_long$value)

#preprocess gender data
gender_sub <- as.data.frame(s1) %>% select(NAME,SE_T003_01, SE_T003_02, SE_T003_00)
colnames(gender_sub) <- c("NAME", "Male", "Female", "Total")
gender_sub$Male <- as.integer(gender_sub$Male)
gender_sub$Female <- as.integer(gender_sub$Female)
gender_sub$Total <- as.integer(gender_sub$Total)
gender_sub_long <- melt(gender_sub, id.vars=c("NAME", "Total"))

#create map popup
s1$popup <- paste("<b>",s1$NAMELSAD,"</b><br>",
                  "Population: ",s1$SE_T003_00,sep ="")


server <- function(input, output, session) {
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map 
  output$map <- renderLeaflet({
    leaflet()  %>%
      addTiles()  %>%
      addPolygons(data = s1, layer=~NAME, stroke = TRUE, fillOpacity = .35,  popup = ~popup)
  })
  
  # store the click
  observeEvent(input$map_shape_click, {
    p<- input$map_shape_click 
    print((p)) 
  }
  ) 
  
  # Make a age barplot  depending of the selected point
  
  output$barpop <- renderPlot({
    
    population_sub_long2 <- population_sub_long %>% 
                               filter (NAME==ifelse(is.null(input$map_shape_click$id), NAME,input$map_shape_click$id))
  
     # filter (NAME==ifelse(is.null(input$map_shape_click$id), NAME,input$map_shape_click$id))
    
    # draw the histogram with the specified number of bins
    ggplot(population_sub_long2, aes(x=variable, y=value))+
      geom_bar(stat = "identity", position = "stack", fill="royalblue2") +
      scale_x_discrete("Age group")+ theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1), 
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))+ 
      #\ylim(0, 1400) +
      labs (y= "Population", title= "Population Age Distribution", 
            subtitle= ifelse(is.null(input$map_shape_click$id),
                     "Plot of Census Tract 15", 
                        (paste("Plot of Census Tract",input$map_shape_click$id))))
  })
  
  # Make a gender barplot  depending of the selected point
  
  output$bargen <- renderPlot({
    
    gender_sub_long2 <- gender_sub_long %>% 
      filter (NAME==ifelse(is.null(input$map_shape_click$id), NAME,input$map_shape_click$id))
    
    ggplot(gender_sub_long2, aes(x=NAME, y=(value/Total), fill=variable), label=value)+ 
      geom_bar(position = "fillkiva",stat = "identity", fill=c("salmon", "royalblue3"))  +
      theme_void() +
      theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+ 
      geom_text(aes(label = paste(variable,"", round(value/Total, 4)*100,"%")), position = position_stack(vjust = .5))+ 
      coord_flip()+ 
      labs ( title= "Population Gender Distribution", 
             subtitle= ifelse(is.null(input$map_shape_click$id),
                    "Plot of Census Tract 15", 
                    (paste("Plot of Census Tract",input$map_shape_click$id))))
  
  })
}

ui <- fluidPage(
  headerPanel("Select a Census Tract"),
  br(),
  column(7,leafletOutput("map", height="500px")),
  column(5,plotOutput("barpop", height="300px"),br(), plotOutput("bargen", height="200px")),
  br()
)

shinyApp(ui = ui, server = server)

