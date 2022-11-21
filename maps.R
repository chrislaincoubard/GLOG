#rm(list=ls())

test = function(){
  library(ggplot2)
  library(tidyverse)
  
  setwd("C:/Users/Elena/Desktop/Mes Documents/M2/GLOG/projet")
  
  data = read.csv("data/sample2.csv", sep=";", header=T, fileEncoding = "UTF-8-BOM")
  mapdata = map_data("world")
  mapdata = left_join(mapdata, data, by="region")
  mapdata = mapdata[!is.na(mapdata$cases),]
  
  mapdata_europe = mapdata[mapdata$long>-21 & mapdata$long<35 & mapdata$lat>34 & mapdata$lat<71,]
  
  map1=ggplot(mapdata_europe, aes(x=long, y=lat, group=group))+
    geom_polygon(aes(fill=cases),color="black")
  map1
  
  map2 = map1 + scale_fill_gradient(name="#cases", low="ivory", high="red", na.value="grey50")+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          rect=element_blank())
  map2
  
  mapdata[mapdata$region=="Russia",]
  
  #-21, 25 / 68, 34
  
}


worldMaps <- function(df, world_data, data_type, period=NULL, indicator=NULL){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[df$Indicator == indicator & df$DataType == data_type & df$Period == period,]
  plotdf <- plotdf[!is.na(plotdf$ISO3), ]
  plotdf <- df[]
  
  # Add the data the user wants to see to the geographical world data
  world_data['DataType'] <- rep(data_type, nrow(world_data))
  world_data['Period'] <- rep(period, nrow(world_data))
  world_data['Indicator'] <- rep(indicator, nrow(world_data))
  world_data['Value'] <- plotdf$Value[match(world_data$ISO3, plotdf$ISO3)]
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(data_type == "Childlessness", "United Nations" , "World Bank"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}