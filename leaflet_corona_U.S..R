library(shiny)
library(leaflet)
library(leaflet.providers)
library(readr)
library(dplyr)

covid <- read_csv("06-28-2020.csv")




#naming longitude and latitude so leaflet can read the columns
covid <- covid %>% rename(lng  = Long_, lat = Lat) %>% 

                   mutate(severe = ifelse(covid$Confirmed >= 100000, TRUE, FALSE),
                   circlesize = ifelse(covid$Confirmed <= 10000, 10,
                                    ifelse(covid$Confirmed <= 30000, 15,
                                       ifelse(covid$Confirmed <= 50000, 20,30
                                       ))))



#color factor states with high number of corona cases
binpal <- colorBin(c("blue", "purple", "orange", "red"), covid$Confirmed, bins = 4, pretty = F, reverse = F)

ui <- bootstrapPage(
  
  #map layout  
  leafletOutput('map', width = "200%", height = 1000) ,
  #adding a slider input for users to view number of cases
  absolutePanel(top =10, right = 30, id = 'controls', 
                sliderInput('cases', 'Number of cases', 1, 500000, 5000)
                
  )
  
)



server <- function(input, output, session){
  
  #Filter data
  data <- reactive({
    covid %>% filter(Confirmed <= input$cases)
  })
  
  observe({
    df = data()
    leafletProxy("map", data = df) %>% 
    addProviderTiles(providers$Stamen.Toner)
      
   })
  output$map <-  renderLeaflet({
    
      #adding tiles 
     m <- data() %>% leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>%
      #set view for the US and circle markers
      setView(-95.7129, 37.0902, zoom = 4) %>%
      addCircleMarkers(weight = ~(Deaths/1000),
                        label = ~paste(Province_State, '\n', (as.character(Confirmed)), 'cases, ',
                                       as.character(Deaths), 'deaths'), color = ~binpal(covid$Confirmed),
                        radius = ~circlesize ) 
     m 
  })
  
}


shinyApp(ui, server)
