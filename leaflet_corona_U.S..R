library(shiny)
library(leaflet)
library(leaflet.providers)
library(readr)
library(tidyverse)


covid <- read_csv("06-02-2020.csv")
#rename Long_ and Lat columns so Leaflet can recgonize the coordintes 
covid <- covid %>% rename(lng  = Long_, lat = Lat) %>% mutate(severe = ifelse(covid$Confirmed >= 100000, TRUE, FALSE),
                                                        circlesize = ifelse(covid$Confirmed <= 10000, 10,
                                                                 ifelse(covid$Confirmed <= 30000, 15,
                                                                 ifelse(covid$Confirmed <= 50000, 20,30
                                                                                     ))))
                                                                              
                                                                              
                                          
#color factor states with high number of corona cases
pal <- colorFactor(c("blue", "red"), covid$severe)


ui <- bootstrapPage(
  
#map layout  
leafletOutput('map', width = "200%", height = 1000) ,
#adding a slider input for users to view number of cases
absolutePanel(top =10, right = 30, id = 'controls', 
              sliderInput('cases', 'Number of cases', 1, 500000, 5000)
  
)
  
)



server <- function(input, output, session){

  output$map <-  renderLeaflet({
#taking input from user
covid %>% filter(Confirmed <= input$cases) %>%
 #adding tiles 
 leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>%
  #set view for the US and circle markers
  setView(-95.7129, 37.0902, zoom = 4) %>% addCircleMarkers( weight = ~(Deaths/1000),
                                popup = ~paste(Province_State, '\n', (as.character(Confirmed)), 'cases, ',
                                as.character(Deaths), 'deaths'), color = ~pal(severe),
                                radius = ~circlesize
                                         
     )
})
    
}


shinyApp(ui, server)






