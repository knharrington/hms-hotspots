################################################################################
# This script is the server for the HMS app
# capabilities include: view maps and customization options

################################################################################
#remove(list = ls())

function(input, output, session) {
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # Handle user authentication by only showing the main user interface when credentials are satisfied
  shinyjs::hide(id="main_ui")
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # Dynamically display the UI according to login permissions
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      session$userData$user_id <- credentials()$info$user
      shinyjs::show(id = "main_ui")  # Show UI when logged in
    } else {
      shinyjs::hide(id = "main_ui")  # Hide UI on logout
    }
  })
  
# Create modal dialog as a welcome/landing page
  observe({
    req(credentials()$info)
    showModal(
      ui = modalDialog(
        title = "Welcome to the Bluefin Tuna Bycatch Reduction Mapping Application",
        tags$div(
          tags$p("This app allows you to filter data on a map, record new observations."),
          tags$h4("How to Use the App:"),
          tags$ul(
            tags$li("Update map layers in the settings button to view data on the map."),
            tags$li("Share your observations of bluefin tuna using the red x-mark button."),
            tags$li("Share your observations of good catch using the blue check-mark button.")
          ),
        ),
        footer = tags$p(style="text-align:center;", tags$em("Data collected through this app is confidential and only accessible to approved members.")),
        easyClose = TRUE,
        fade = TRUE
      )
    )
  })

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Create a reactiveValues object to store the observation database
  data_store <- reactiveValues(data = NULL)  

# Function to fetch and update the data (e.g., from Google Sheets)
  update_sheet_data <- function() {
    tryCatch({
      sheet_data <- read_sheet(ss = sheet_id, sheet = "main")
      data_store$data <- sheet_data  
      #show_alert("Data updated successfully", type="success", btn_colors = "#3b8dbc")
      #showNotification("Data updated successfully", type="message")
    }, error = function(e) {
      showNotification("Error reading Google Sheet", type="error")
      print(paste("Error reading sheet:", e))
    })
  }  
  
  # Fetch initial data on app start-up
  update_sheet_data()
  
  # Create a reactive value to store the last clicked button
  last_clicked <- reactiveVal(NULL)
  
  # When Good Catch is clicked, store "good catch"
  observeEvent(input$good_catch, {
    last_clicked("good catch")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # When Bad Catch is clicked, store "bluefin tuna"
  observeEvent(input$bad_catch, {
    last_clicked("bluefin tuna")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Perform error checks if data is confirmed
  observeEvent(last_clicked(), {
    req(last_clicked())  # Ensure a value exists
    
      timestamp <- Sys.time()
      
      user_id <- session$userData$user_id
      
        if (input$geolocation == TRUE) {
          lon <- as.numeric(input$long)
          lat <- as.numeric(input$lat)
        } else {  
          lon <- -88
          lat <- 29
          show_alert("Error writing data",
                     text = "Please check your browser settings to allow location access.",
                     type="error", btn_colors = "#dd4b39")
          return(NULL)
        }
      
      # Compile responses into a data frame
      response_data <- data.frame(
        user_id = as.character(user_id),
        timestamp = timestamp,
        latitude = lat,
        longitude = lon,
        observation = last_clicked()
      ) 
      
      values <- read_sheet(ss = sheet_id, sheet="main")
      
      # Merge the new responses with the existing Google Sheet
      tryCatch({
        if (nrow(values) == 0) {
          sheet_write(data = response_data,
                      ss = sheet_id,
                      sheet = "main")
        } else {
          sheet_append(data = response_data,
                       ss = sheet_id,
                       sheet = "main")
        }
        # Handle connectivity errors
        update_sheet_data()
      }, error = function(e) {
        showNotification("Error writing to Google Sheet", type="error")
      })
      
      show_alert("Data updated successfully", type="success", btn_colors = "#3b8dbc")
      
      shinyjs::enable("submit")
      
  }, ignoreNULL=TRUE) # End input submit
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# filter data according to inputs
filtered <- reactive({
  noaa_data %>% filter(
    Days_Report <= input$days[1]) %>%
    group_by(UNIQUE_RETRIEVAL, SPECIES_NAME) %>%
    mutate(
      UNIQUE_RET_LAT = round(mean(CENTROID_LAT), 6),
      UNIQUE_RET_LON = round(mean(CENTROID_LON), 6)
    )
})

# Convert the data frame to a spatial object
filtered_sf <- reactive({st_as_sf(filtered(), coords = c("UNIQUE_RET_LON", "UNIQUE_RET_LAT"), crs = st_crs(gridshp))})

# Perform the spatial join
grid_join <- reactive({setDT(st_join(filtered_sf(), gridshp, join = st_intersects))})

# Filter and aggregate the data
filtered_grid <- reactive({
  grid_join() %>%
    group_by(GRID_ID) %>%
    dplyr::reframe(
      NUM_BLUEFIN = sum(NUM_FISH[SPECIES_NAME == "TUNA BLUEFIN"], na.rm = TRUE),
      NUM_OTHER = sum(NUM_FISH[SPECIES_NAME != "TUNA BLUEFIN"], na.rm = TRUE),
      unique_vessel_ids = n_distinct(VESSEL_ID)  # Count unique VESSEL_IDs in each grid
    ) %>%
    mutate(
      RATIO = ifelse(NUM_OTHER > 0, NUM_BLUEFIN / NUM_OTHER, NUM_BLUEFIN), 
      CLASSIFICATION = case_when(
        NUM_BLUEFIN >= 1 ~ "Bad",
        TRUE ~ "Good"
      )
    ) %>%
    filter(
      unique_vessel_ids >= 3,  # at least 3 unique VESSEL_IDs contributing
      GRID_ID != "NA")
})

# Merge the results back with the grid shapefile
gridvalues <- reactive({st_as_sf(merge(x = gridshp, y = filtered_grid(), by = "GRID_ID", all.x = FALSE))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Options: "Bluefin Tuna Predictions", "Bluefin Tuna Interactions", "Good Catch", "Currents", "Graticule
# Map with proxy
output$map <- renderLeaflet({
  req(credentials()$info)
  leaflet() %>%
    addProviderTiles("Esri.NatGeoWorldMap", options = providerTileOptions(minZoom = 6, maxZoom = 12)) %>%
    setView(lng=-90, lat=27.5, zoom=7)  %>%
    addScaleBar(position = 'topleft',
                options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) %>%
    addSimpleGraticule(interval = 1, 
                       group = "Graticule") %>%
    addMarkers(lng=-88, lat=27.5, icon=boat_icon) %>%
    addControl(html=html_legend, position="topright") %>%
    # addPolygons( 
    #   data = gridshp15,
    #   color="black",
    #   fillColor = "transparent",
    #   weight=0.33,
    #   fillOpacity=0.5) %>%
    addPolygons(data = gridvalues()[gridvalues()$CLASSIFICATION == "Bad",],
                fillColor = "#C55301",
                weight = 3,
                opacity = 1,
                fillOpacity = 0.25,
                color = "#C55301",
                group = "Bluefin Tuna Interactions") %>%
    addPolygons(data = gridvalues()[gridvalues()$CLASSIFICATION == "Good",],
                fillColor = "#036BA1",
                weight = 3,
                opacity = 1,
                fillOpacity = 0.25,
                color = "#036BA1",
                group = "Good Catch") %>%
    addVelocity(content=water_currents,
                group = "Currents",
                options= velocityOptions(
                  speedUnit="m/s",
                  maxVelocity= 2.0,
                  velocityScale= 0.5,
                  velocityType="Water"
                ))
})

# listen for clicking update button
observeEvent(input$update, {
  proxy <- leafletProxy("map") %>%
    clearShapes() %>%
    clearControls() %>%
    clearHeatmap() %>%
    # addPolygons( 
    #   data = gridshp15,
    #   color="black",
    #   fillColor = "transparent",
    #   weight=0.33,
    #   fillOpacity=0.5) %>%
    addMarkers(lng=-88, lat=27.5, icon=boat_icon) %>%
    addControl(html=html_legend, position="topright")
  
  # Predictions
  if ("Bluefin Tuna Predictions" %in% input$layer) {
    show_alert("Layer Unavailable", text="Predictive modeling
               for bluefin tuna interactions is currently unavailable.", type="error", btn_colors="#C55301")
  } 
  
  # Interactions
  if ("Bluefin Tuna Interactions" %in% input$layer) {
    proxy %>%
      clearGroup("Bluefin Tuna Interactions") %>%
      addPolygons(data = gridvalues()[gridvalues()$CLASSIFICATION == "Bad",],
                  fillColor = "#C55301",
                  weight = 3,
                  opacity = 1,
                  fillOpacity = 0.25,
                  color = "#C55301",
                  group = "Bluefin Tuna Interactions") 
  } else {
    proxy %>%
      clearGroup("Bluefin Tuna Interactions")
  }
  
  # Good Catch
  if ("Good Catch" %in% input$layer) {
    proxy %>%
      clearGroup("Good Catch") %>%
      addPolygons(data = gridvalues()[gridvalues()$CLASSIFICATION == "Good",],
                  fillColor = "#036BA1",
                  weight = 3,
                  opacity = 1,
                  fillOpacity = 0.25,
                  color = "#036BA1",
                  group = "Good Catch")
  } else {
    proxy %>%
      clearGroup("Good Catch")
  }
  
  # Graticule
  if ("Graticule" %in% input$layer) {
    proxy %>%
      clearGroup("Graticule") %>%
      addSimpleGraticule(interval = 1, 
                         group = "Graticule")
  } else {
    proxy %>%
      clearGroup("Graticule")
  }
  
  # Currents
  if ("Currents" %in% input$layer) {
    proxy %>%
      clearGroup("Currents") %>%
      addVelocity(content=water_currents,
                  group = "Currents",
                  options= velocityOptions(
                    speedUnit="m/s",
                    maxVelocity= 2.0,
                    velocityScale= 0.5,
                    velocityType="Water"
                  ))
  } else {
    proxy %>%
      clearGroup("Currents")
  }
  
}) # end observe


} #end server



 


