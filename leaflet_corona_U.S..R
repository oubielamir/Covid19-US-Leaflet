library(shiny)
library(leaflet)
library(leaflet.providers)
library(readr)
library(tidyverse)


covid <- read_csv("05-27-2020.csv")

covid <- covid %>% rename(lng  = Long_, lat = Lat) %>% mutate(severe = ifelse(covid$Confirmed >= 100000, TRUE, FALSE),
                                                        circlesize = ifelse(covid$Confirmed <= 10000, 10,
                                                                 ifelse(covid$Confirmed <= 30000, 15,
                                                                 ifelse(covid$Confirmed <= 50000, 20,30
                                                                                     ))))
                                                                              
                                                                              
                                          

pal <- colorFactor(c("blue", "red"), covid$severe)


ui <- bootstrapPage(
  
leafletOutput('map', width = "200%", height = 1000) ,

absolutePanel(top =10, right = 30, id = 'controls', 
              sliderInput('cases', 'Number of cases', 1, 500000, 5000)
  
)
  
)



server <- function(input, output, session){

  output$map <-  renderLeaflet({
covid %>% filter(Confirmed <= input$cases) %>%
#%>% filter(Deaths <= 100) 
 leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>%
  #fitBounds(~min(covid$Lat), ~min(covid$Long_), ~max(covid$Lat), ~max(covid$Long_)) %>%
  setView(-95.7129, 37.0902, zoom = 4) %>% addCircleMarkers( weight = ~(Deaths/1000),
                                popup = ~paste(Province_State, '\n', (as.character(Confirmed)), 'cases, ',
                                as.character(Deaths), 'deaths'), color = ~pal(severe),
                                radius = ~circlesize
                                         
     )
})
    
}


shinyApp(ui, server)






