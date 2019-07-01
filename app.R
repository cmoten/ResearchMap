#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(htmltools)
library(dplyr)
library(DT)

# Define UI for application that contains a map, filter, and data table
ui <- dashboardPage(
  dashboardHeader(title = "Research Institutions"),
  dashboardSidebar(
    menuItem("Institution Map", tabName = "institution", icon = icon("globe"))
  ),
  dashboardBody(
    fluidRow(
      #Boxes for the map and institution table
      column(width = 9,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("institutionmap", height = 500)
                 ),
             box(width = NULL,
                 DTOutput("institutiontable"))
      ),
      #Filter for the map
      column(width = 3,
             box(width = NULL, status = "primary",
                 uiOutput("filterinstitution"),
                 selectizeInput("cityselect", "Select a City", choices = "")
                 )
             )
    )

  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  map_data <- readRDS("data/rds/research-institutions.RDS") %>%
    filter(Location != "")
  no_data <- is.na(map_data$Latitude)
  map_data <- map_data[!no_data, ]
  map_data$content <- paste('<b>', map_data$Institution, '</b>',
                            map_data$Location,
                            map_data$Type,
                            sep="<br>")
  
  link_data <- readRDS("data/rds/research-institutions-links.RDS")
  
  city_data <- readRDS("data/rds/city-locations.RDS")
  
  map_filtered <- reactive({
    if(input$cityselect == "All"){
      map_data
    } else{
      map_data[map_data$Location == input$cityselect, ]
    }
    
  })
  

   observe({
    updateSelectizeInput(session,
                      "cityselect",
                      choices = c("All",sort(unique(map_data$Location))) ,
                      selected = "All")
  })
   

   output$institutionmap <- renderLeaflet({

     
     map_data %>% leaflet() %>% addTiles() %>%
       setView(lng = -38.32, lat = 37.16, zoom = 2 ) %>%
       setMaxBounds(lng1 = ~min(Longitude),
                    lat1 = ~min(Latitude),
                    lng2 = ~max(Longitude),
                    lat2 = ~max(Latitude)) %>%
       addMarkers(lng = ~Longitude, lat = ~Latitude,
                  popup = ~as.character(content),
                  label = ~as.character(Institution))

   })
   
   output$institutiontable <- renderDT({
     datatable(link_data, escape = FALSE)
   })
  
  observe({

    if(input$cityselect == "All"){
      leafletProxy('institutionmap', data = map_filtered()) %>% clearMarkers() %>%
        setView(lng = -38.32, lat = 37.16, zoom = 2 ) %>%
        addMarkers(lng = ~Longitude, lat = ~Latitude,
                   popup = ~as.character(content),
                   label = ~as.character(Institution))


    } else{
      leafletProxy("institutionmap", data = map_filtered()) %>% clearMarkers() %>%
        setView(lng = city_data$lon[city_data$Location == input$cityselect],
                lat = city_data$lat[city_data$Location == input$cityselect],
                zoom = 12) %>%
        addMarkers(lng = ~Longitude, lat = ~Latitude,
                   popup = ~as.character(content))
    }


  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

