library(shiny)
library(leaflet)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "test-styles.css")
  ),
  
  leafletOutput("map", height = "100vh"),
  
  # Control Panel with a Slider
  div(id = "controls",
      sliderInput("year", "Year",
                  min = 2016, max = 2023, value = 2016, sep = "", round = TRUE, step = 1,
                  animate = TRUE)
  ),
  
  # Floating Buttons
  actionButton("zoom_in", "+", class = "map-button-in"),
  actionButton("zoom_out", "zoom out", class = "map-button-out"),
  
  tags$div(id = "map-butt", actionBttn("map_butt", "another", style = "material-circle", icon = icon("check"))),
  
  # Ensure dropdownButton() is wrapped in a div with an ID
  tags$div(id = "dropitdown",
           dropdownButton(
             tags$h3("List of Inputs"),
             
             selectInput(inputId = 'xcol',
                         label = 'X Variable',
                         choices = names(iris)),
             
             selectInput(inputId = 'ycol',
                         label = 'Y Variable',
                         choices = names(iris),
                         selected = names(iris)[[2]]),
             
             sliderInput(inputId = 'clusters',
                         label = 'Cluster count',
                         value = 3,
                         min = 1,
                         max = 9),
             
             circle = TRUE, status = "danger",
             icon = icon("gear"), width = "300px",
             
             tooltip = tooltipOptions(title = "Click to see inputs !")
           )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -90, lat = 30, zoom = 4)
  })
  
  observeEvent(input$zoom_in, {
    leafletProxy("map") %>% setView(lng = -90, lat = 30, zoom = 6)
  })
  
  observeEvent(input$zoom_out, {
    leafletProxy("map") %>% setView(lng = -90, lat = 30, zoom = 3)
  })
}

shinyApp(ui, server)
