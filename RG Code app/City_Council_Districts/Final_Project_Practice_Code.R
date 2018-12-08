#DV Final Project practice code

#set the working directory
setwd("~/Desktop/City_Council_Districts/")

#load the necessary packages
library(tidyverse)
library(leaflet)
library(rgdal)
library(ggmap)

#load the data
districts <- readOGR(dsn = "~/Desktop/City_Council_Districts", 
                     layer = "City_Council_Districts",
                     stringsAsFactors = FALSE)

code_violations <- read.csv("~/Documents/GitHub/Data-Viz-2018-Fall/FinalProject/Code_Enforcement_Cases.csv")
code_violations_active <- code_violations %>%
  filter(Case_Status_Code %in% "AC")

code_violations_active

#view the data
districts@data

districts@data$Dist <- as.factor(districts@data$Dist)

#plot a basic map of the data
plot(districts)

districts$popup <- paste("Council Member: ", districts$Council_Me)

#create a leaflet map
leaflet(data = districts) %>%
  addTiles() %>%
  addPolygons(color = "green", 
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              fillColor = ~colorFactor(palette = "YlGn", domain = Dist)(Dist),
              popup = ~popup)
  
              