################################################################################
# This script is the server for the HMS app
# capabilities include: view maps and customization options

################################################################################
#remove(list = ls())

function(input, output, session) {
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#bs_themer()
  
# Make a plotly graph for top species caught
output$topspeciesbar <- renderPlotly({
  bar_plotly
  })  

  # Function to filter species and update dataset
  gam_data <- reactive({
    req(length(input$species) > 0)
    gdata <- if ("ALL SHARKS" %in% input$species) {
      noaa_data %>% filter(SPECIES_NAME %in% setdiff(input$species, "ALL SHARKS") |
                             grepl("SHARK", SPECIES_NAME))
    } else {
      noaa_data %>% filter(SPECIES_NAME %in% input$species)
    }
    gdata <- gdata %>%
      group_by(UNIQUE_RETRIEVAL) %>%
      mutate(
        FISH_PER_KM = NUM_FISH / HAUL_LENGTH_KM,
        FISH_PER_10KM = FISH_PER_KM * 10,
        SOAK_TIME = as.numeric(difftime(BEGIN_HAUL_DATE_TIME, END_SET_DATE_TIME, units = "hours")),
        FISH_PER_SOAK_HR = NUM_FISH / SOAK_TIME,
        FISH_PER_8SOAK_HR = FISH_PER_SOAK_HR * 8,
        MID_HAUL_NUMERIC = as.numeric(MID_HAUL_UTC)
      ) %>%
      ungroup()
  })
  
  # Fit the GAM model only when `gam_data()` is available
  gam_model <- reactive({
    req(gam_data())  # Prevents errors when gam_data() is NULL
    gam(FISH_PER_10KM ~ s(MID_HAUL_NUMERIC, bs = "cs"), data = gam_data())
  })
  
  # Predict fitted values
  gam_pred <- reactive({
    req(gam_data(), gam_model())  # Ensure data and model are available before predicting
    gam_data() %>%
      mutate(y_pred = predict(gam_model(), newdata = gam_data(), type = "response"))
  })

# Make a Plotly GAM visualization
output$species_gam <- renderPlotly({
  if (length(input$species) == 0) {
    validate(need(FALSE, "Please select at least one species to display the GAM."))
  } else {
  req(gam_data(), gam_pred())  # Ensure valid data before plotting
  plot_ly() %>%
    add_markers(data = gam_data(), x = ~MID_HAUL_UTC, y = ~FISH_PER_10KM, 
                marker = list(color= "#2c3e50", opacity = 0.5), hoverinfo="none") %>%
    add_lines(data = gam_pred(), x = ~MID_HAUL_UTC, y = ~y_pred, 
                line = list(color= "#18bc9c",width = 2), hoverinfo="none") %>%
    layout(
      xaxis = list(title = "", tickformat = "%Y-%m", type = "date", tickmode = "auto", nticks = 16),
      yaxis = list(title = "Catch per 10 km"),
      showlegend=FALSE,
      hovermode=FALSE
    )
  }
})

# Make Data set grid-reactive
gridshp <- reactive({
    if (input$radio_grid == "10 Min Square") {
      return(gridshp10)
  } else if (input$radio_grid == "15 Min Square") {
      return(gridshp15)
  } else if (input$radio_grid == "10 Min Hexagon") {
      return(gridshp10h)
  }
})
 
filtered_prop <- reactive({
  fdata <- if ("ALL SHARKS" %in% input$species) {
    noaa_data %>% filter(SPECIES_NAME %in% setdiff(input$species, "ALL SHARKS") |
                           grepl("SHARK", SPECIES_NAME))
  } else {
    noaa_data %>% filter(SPECIES_NAME %in% input$species)
  }
  fdata %>% filter(Days_Report >= input$days[2],
                   Days_Report <= input$days[1],
                   MOON_PHASE %in% input$lunar)
})

# calculate reactive grid values before merge
noaa_prop <- reactive({
  filtered_prop() %>%
group_by(UNIQUE_RETRIEVAL, SPECIES_NAME) %>%
  mutate(
    NUM_FISH = NUM_FISH,
    NUM_DAM = sum(NUM_FISH[CONDITION %in% c('DAMAGED')]),
    PROP_DAM = NUM_DAM / NUM_FISH,
    FISH_PER_KM = NUM_FISH / HAUL_LENGTH_KM,
    FISH_PER_10KM = FISH_PER_KM * 10,
    SOAK_TIME = as.numeric(difftime(BEGIN_HAUL_DATE_TIME, END_SET_DATE_TIME, units = "hours")),
    FISH_PER_SOAK_HR = NUM_FISH / SOAK_TIME,
    FISH_PER_8SOAK_HR = FISH_PER_SOAK_HR * 8,
    UNIQUE_RET_LAT = round(mean(CENTROID_LAT), 6),
    UNIQUE_RET_LON = round(mean(CENTROID_LON), 6)
  )  
})

# Convert the data frame to a spatial object
noaa_prop_sf <- reactive({st_as_sf(noaa_prop(), coords = c("UNIQUE_RET_LON", "UNIQUE_RET_LAT"), crs = st_crs(gridshp()))})

# Perform the spatial join
grid_join <- reactive({setDT(st_join(noaa_prop_sf(), gridshp(), join = st_intersects))})

# Filter and aggregate the data
filtered_data <- reactive({
  grid_join() %>%
  group_by(GRID_ID) %>%
  dplyr::reframe(
    PROP_DAM.mean = mean(PROP_DAM, na.rm = TRUE),
    FISH_PER_10KM.mean = mean(FISH_PER_10KM, na.rm = TRUE),
    FISH_PER_8SOAK_HR.mean = mean(FISH_PER_8SOAK_HR, na.rm = TRUE),
    num_points = n(),  # Count the number of points in each grid
    unique_vessel_ids = n_distinct(VESSEL_ID)  # Count unique VESSEL_IDs in each grid
  ) %>%
  filter(
    num_points >= 3, 
    unique_vessel_ids >= 3,  # Apply the filtering condition: at least 3 points and at least 3 unique VESSEL_IDs
    GRID_ID != "NA",
    PROP_DAM.mean != 'Inf') %>%
  mutate(classification = case_when(
    PROP_DAM.mean == 0 ~ 'None',
    PROP_DAM.mean <= 0.10 ~ 'Moderate',
    PROP_DAM.mean > 0.10 ~ 'High')
  )
})

# Merge the results back with the grid shapefile
gridvalues <- reactive({st_as_sf(merge(x = gridshp(), y = filtered_data(), by = "GRID_ID", all.x = FALSE))})

# Options: "Catch per 10 km", "Catch per 8 soak hrs", "Depredation Intensity", "Catch Density"
# Map with proxy
output$examplemap <- renderLeaflet({
  showNotification("Update map in order to view data", duration = 30, closeButton = TRUE)
  leaflet() %>%
    addProviderTiles("Esri.NatGeoWorldMap", options = providerTileOptions(minZoom = 6, maxZoom = 12)) %>%
    setView(lng=-90, lat=27.5, zoom=7)  %>%
    addScaleBar(position = 'topleft',
                options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) 
})

# define static palette
colors <- c("#00a65a", "#f39c12", "#dd4b39")
pro_levels <- c("None", "Moderate", "High")
pro_pal <- colorFactor(colors, levels = pro_levels, domain = c("None", "Moderate", "High"))


fakegrid <- reactive({
  gridshp() %>%
    mutate(class = case_when( GRID_ID %in% c(713,646,647,852,781,782,581,865,708,565,496,501,434,435,921) ~ "red") ) %>%
  filter(class != "NA")
})

fakemark <- data.frame(
  lat = c(27.551,27.622,26.756, 26.823,26.489, 27.4),
  lon = c(-92.015,-92.144,-90.025,-93.758,-93.683, -91.6)
)

marker_icon <- makeAwesomeIcon(icon = "check",
                               library = "fa",
                               markerColor = "green",
                               iconColor = "#FFFFFF")

# listen for clicking update button
observeEvent(input$update, {
  
  if (length(input$lunar) < 1 | length(input$species) < 1) {
  show_alert("Please ensure at least one species and lunar phase is selected.", type="error", showCloseButton=TRUE)
  } else {
  
  # Define dynamic palettes and poppers
  km_pal <- colorNumeric(palette=colorRampPalette(c("#F5F5F5","#2c3e50", "#1C2838"))(10), domain = gridvalues()$FISH_PER_10KM.mean)
  hr_pal <- colorNumeric(palette=colorRampPalette(c("#F5F5F5","#f39c12", "#8C570A"))(10), domain = gridvalues()$FISH_PER_8SOAK_HR.mean)
  popper <- paste0("<strong>Proportion Damaged: </strong>", round(gridvalues()$PROP_DAM.mean, digits = 2))
  pop_km <- paste0("<strong>Catch per 10 km: </strong>", round(gridvalues()$FISH_PER_10KM.mean, digits=2))
  pop_hr <- paste0("<strong>Catch per 8 soak hrs: </strong>", round(gridvalues()$FISH_PER_8SOAK_HR.mean, digits=2))

  proxy <- leafletProxy("examplemap")
  
  proxy %>% 
    clearHeatmap() %>%
    clearShapes() %>%
    clearControls() %>%
    leafem::addMouseCoordinates() %>%
    addPolygons( 
      data = gridshp(),
      color="black",
      fillColor = "transparent",
      weight=0.33,
      fillOpacity=0.5,
      popup = paste0(gridshp()$GRID_ID)
    ) %>%
    addPolygons( 
      data = fakegrid(),
      color="#dd4b39",
      fillColor = "transparent",
      weight=4,
      fillOpacity=0.5,
      popup = paste0(gridshp()$GRID_ID)
    ) %>% 
    addAwesomeMarkers(data = fakemark,
                      lng = ~lon,
                      lat = ~lat,
                      icon = marker_icon) %>%
    addSimpleGraticule(interval = 1, 
                       group = "Graticule") %>%
    addLayersControl(position = "topleft", overlayGroups = c("Graticule"), 
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addMarkers(lng=-88, lat=27.5, icon=boat_icon) %>%
    addControl(html=html_legend, position="topright")
  
  if (input$radio_layer == "Catch per 10 km") {
    proxy %>%
      addPolygons(data = gridvalues(),
                  fillColor = ~km_pal(FISH_PER_10KM.mean),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~pop_km,
                  group = "Catch per 10 km") %>%
      addLegend(pal = km_pal,
                values = gridvalues()$FISH_PER_10KM.mean,
                opacity = 1,
                title = HTML("Catch per<br>10 km"),
                group = "Catch per 10 km")
  } else if (input$radio_layer == "Catch per 8 soak hrs") {
    proxy %>%
      addPolygons(data = gridvalues(),
                  fillColor = ~hr_pal(FISH_PER_8SOAK_HR.mean),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~pop_hr,
                  group = "Catch per 8 soak hrs") %>%
      addLegend(pal = hr_pal,
                values = gridvalues()$FISH_PER_8SOAK_HR.mean,
                opacity = 1,
                title = HTML("Catch per<br>8 soak hrs"),
                group = "Catch per 8 soak hrs")
  } else if (input$radio_layer == "Depredation Intensity") {
    proxy %>%
      addPolygons(data = gridvalues(),
                  fillColor = ~pro_pal(classification),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~popper,
                  group = "Depredation Intensity") %>%
      addLegend(position = 'topright',
                pal = pro_pal,
                values = gridvalues()$classification,
                opacity = 1,
                title = HTML("Depredation<br>Intensity"),
                group = "Depredation Intensity")
  } else if (input$radio_layer == "Catch Density") {
    proxy %>%
      addHeatmap(
        data = filtered_prop(),
        lng = filtered_prop()$CENTROID_LON,
        lat = filtered_prop()$CENTROID_LAT,
        #gradient = "Spectral",
        intensity = ~NUM_FISH,
        blur = 35,
        #max = 0.05,
        radius = 20,
        group = "Catch Density"
      ) 
  }
  
  } # end lunar check
}) # end observe

############### PRINT NUMBER OF OBSERVATIONS DISPLAYED IN THE MAP ##############
  output$text_obs <- renderText({
    format(sum(filtered_prop()$NUM_FISH), big.mark=",")
  })

} #end server



 


