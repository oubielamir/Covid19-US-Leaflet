library(shiny)
library(leaflet)
library(leaflet.providers)
library(readr)
library(tidyverse)


covid <- read_csv("04-28-2020.csv")

covid <- covid %>% rename(lng  = Long_, lat = Lat)



ui <- bootstrapPage(
  
leafletOutput('map', width = "200%", height = 1000)  
  
  
  
)



server <- function(input, output, session){

  output$map <-  renderLeaflet({
covid %>%
#%>% filter(Deaths <= 100) 
 leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>%
  #fitBounds(~min(covid$Lat), ~min(covid$Long_), ~max(covid$Lat), ~max(covid$Long_)) %>%
  setView(-98.58, 39.82, zoom = 1) %>% addCircleMarkers(radius = ~(Confirmed/1000), weight = 1,
                                            popup = paste(covid$Province_State, '\n', (as.character(covid$Confirmed)), 'cases'))
})
    
}


shinyApp(ui, server)


