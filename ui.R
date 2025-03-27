################################################################################
# This script builds the user interface for the HMS app
# capabilities include: viewing maps and customization options
################################################################################

fluidPage(
  tags$head(
    # use custom CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    # Ask for geolocation permissions on start
    tags$script(HTML('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);

        function onError (err) {
        Shiny.onInputChange("geolocation", false);
        }

       function onSuccess (position) {
          setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
          }, 1100)
      }
      });
    '))
  ),
  
  div(id = "main_title", style = "text-align: center;", tags$h2("Pelagic Longline Fishery Hotspot Mapping Application")),
  
  # log in module
  shinyauthr::loginUI("login"),
  
  div(id="main_ui",
  
  # Map
  leafletOutput("map", height = "100vh"),
  
  # Log out
  div(id="logout-container", shinyauthr::logoutUI(id = "logout")),
  
  # Floating Buttons
  div(id = "catch-panel",
    tags$div(class = "catch-panel-title", "Record Fishing Observation"),
    div(id = "button-row",
        div(id = "good-catch", actionBttn("good_catch", style = "material-circle", icon = icon("check"))),
        div(id = "bad-catch", actionBttn("bad_catch", style = "material-circle", icon = icon("xmark")))
    )
  ),
  
  # Dropdown menu inputs
  div(id = "dropitdown",
           dropdownButton(
             
             checkboxGroupInput("layer", "Map Layers", 
                                choices = c("Bluefin Tuna Predictions", "Bluefin Tuna Interactions", "Good Catch", "Currents", "Graticule"),
                                selected = c("Currents", "Graticule")),
             
             sliderInput("days", "Days Since Reporting", min=1, max=14, step=1, value=14),
             
             div(id="update", actionButton("update", "Update Map", icon=icon("refresh"))),
             
             circle = TRUE,
             icon = icon("gear"), width = "300px"
           )
    )
  )
)
