# Class 10 Example

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)

# Data Source: https://data.cityofnewyork.us/Environment/DEP-Green-Infrastructure/spjh-pz7h
greenInf.load <- readOGR("https://data.cityofnewyork.us/api/geospatial/spjh-pz7h?method=export&format=GeoJSON")

icons <- awesomeIconList(
  MS4 = makeAwesomeIcon(icon = "road", library = "fa", markerColor = "gray"),
  Combined = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue"),
  `Non-combined` = makeAwesomeIcon(icon = "tint", library = "fa", markerColor = "green"),
  `On-site management` = makeAwesomeIcon(icon = "building-o", library = "fa", markerColor = "cadetblue")
)

# Define UI for application
ui <- navbarPage("NYC Green Infrastructure",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("sewerSelect",
                                          "Sewer Type",
                                          levels(greenInf.load$sewer_type),
                                          selected = c("MS4", "Non-combined"),
                                          selectize = T,
                                          multiple = T),
                              radioButtons("boroSelect",
                                           "Borough Filter:",
                                           choices = levels(greenInf.load$borough),
                                           selected = "Bronx")
                            ),
                            mainPanel(
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
                              leafletOutput("leaflet")
                              )
                            )
                          ),
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$leaflet <- renderLeaflet({
     leaflet() %>%
       addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google") %>%
       setView(-74.0060, 40.7128, 9)
   })
    
   greenInfInputs <- reactive({
     greenInf <- greenInf.load 
     
      greenInf <- subset(greenInf, borough == input$boroSelect)
     
     if (length(input$sewerSelect) > 0) {
       greenInf <- subset(greenInf, sewer_type %in% input$sewerSelect)
     }
     
     return(greenInf)
   })
   
   observe({
     greenInf <- greenInfInputs()
     
     leafletProxy("leaflet", data = greenInf) %>%
       clearShapes() %>%
       addAwesomeMarkers(icon = ~icons[sewer_type], popup = ~paste0("<b>", project_na, "</b>: ", sewer_type))
   })
   
   output$table <- DT::renderDataTable(greenInfInputs()@data, options = list(scrollX = T))
}

# Run the application 
shinyApp(ui = ui, server = server)

