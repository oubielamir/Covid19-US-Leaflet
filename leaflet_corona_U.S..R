library(shiny)
library(leaflet)
library(leaflet.providers)
library(readr)
library(tidyverse)


covid <- read_csv("05-27-2020.csv")

covid <- covid %>% rename(lng  = Long_, lat = Lat)



ui <- bootstrapPage(
  
leafletOutput('map', width = "200%", height = 1000) ,

absolutePanel(top =10, right = 30, id = 'controls', 
              sliderInput('cases', 'Number of cases', 1, 500000, 3000)
  
)
  
)



server <- function(input, output, session){

  output$map <-  renderLeaflet({
covid %>% filter(Confirmed <= input$cases) %>%
#%>% filter(Deaths <= 100) 
 leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>%
  #fitBounds(~min(covid$Lat), ~min(covid$Long_), ~max(covid$Lat), ~max(covid$Long_)) %>%
  setView(-95.7129, 37.0902, zoom = 4) %>% addCircleMarkers( weight = ~(Deaths/1000),
                                popup = paste(covid$Province_State, '\n', (as.character(covid$Confirmed)), 'cases, ',
                                                        as.character(covid$Deaths), 'deaths')
                                         
     )
})
    
}


shinyApp(ui, server)


