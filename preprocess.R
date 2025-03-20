# preprocessing for the HMS app

# load packages
{
  library(tidyverse)
  #library(plyr)
  library(data.table)
  library(lubridate)
  
  #library(spatial)
  library(sp)
  library(sf)
  #library(spdep)
  #library(sfdep)
  #library(fields)
  
  #library(rasterVis)
  #library(raster)
  library(RColorBrewer)
  
  library(leaflet)
  #library(kableExtra)
  #library(shiny)
  library(plotly)
  #library(htmlwidgets)
  #library(shinyWidgets)
  #library(shinythemes)
  #library(shinycustomloader)
  #library(leaflet.extras)
  #library(leaflet.extras2)
  
  #library(KernSmooth)
  #library(ks)
  
  library(geosphere)
  library(lunar)
  library(suncalc)
  library(oce)
}

{
# Read data
Data.In <- fread("HMS_App/data/NOAA-HMS-data.csv")
noaa_data = Data.In

# Format date/times & unique events
{
  formats <- c("m/d/y I:M p", "m/d/y")
  noaa_data <- noaa_data %>% dplyr::filter(SPECIES_NAME != "" | BEGIN_SET_DATE != "") #| VESSEL_ID != "")
  noaa_data$BEGIN_SET_DATE_TIME <- parse_date_time(noaa_data$BEGIN_SET_DATE, orders=formats)
  noaa_data$END_SET_DATE_TIME <- parse_date_time(noaa_data$END_SET_DATE, orders=formats)
  noaa_data$BEGIN_HAUL_DATE_TIME <- parse_date_time(noaa_data$BEGIN_HAUL_DATE, orders=formats)
  noaa_data$END_HAUL_DATE_TIME <- parse_date_time(noaa_data$END_HAUL_DATE, orders=formats)
  noaa_data$HAUL_DURATION <- as.numeric(difftime(noaa_data$END_HAUL_DATE_TIME, noaa_data$BEGIN_HAUL_DATE_TIME, units = "hours"))
  noaa_data$MID_HAUL_DATE_TIME <- noaa_data$BEGIN_HAUL_DATE_TIME + as.difftime(noaa_data$HAUL_DURATION / 2, units = "hours")
  noaa_data$MID_HAUL_UTC <- with_tz(noaa_data$MID_HAUL_DATE_TIME, tzone = "UTC")
  noaa_data$DEPART_DATE <- mdy(noaa_data$DEPARTURE_DATE)
  noaa_data$LAND_DATE <- mdy(noaa_data$LANDING_DATE)
  noaa_data$SET_YEAR <- year(noaa_data$BEGIN_SET_DATE_TIME)
  noaa_data$UNIQUE_RETRIEVAL <- paste(noaa_data$VESSEL_ID, noaa_data$TRIP_NUMBER, noaa_data$HAUL_NUMBER, sep="_")
  noaa_data$SEA_DAYS <- as.numeric(difftime(noaa_data$LAND_DATE, noaa_data$DEPART_DATE, units = "days"))
}

# Convert degrees minutes to decimal degrees
dec_deg <- function(degree, minute) {
  return(degree + minute / 60)
}
{
  noaa_data$BEGIN_HAUL_LAT <- dec_deg(noaa_data$BEGIN_HAUL_LATITUDE, noaa_data$BEGIN_HAUL_LATITUDE_MINUTES)
  noaa_data$BEGIN_HAUL_LON <- dec_deg(noaa_data$BEGIN_HAUL_LONGITUDE, noaa_data$BEGIN_HAUL_LONGITUDE_MINUTES) * -1
  noaa_data$END_HAUL_LAT <- dec_deg(noaa_data$END_HAUL_LATITUDE, noaa_data$END_HAUL_LATITUDE_MINUTES)
  noaa_data$END_HAUL_LON <- dec_deg(noaa_data$END_HAUL_LONGITUDE, noaa_data$END_HAUL_LONGITUDE_MINUTES) * -1
  noaa_data$CENTROID_LAT <- (noaa_data$BEGIN_HAUL_LAT + noaa_data$END_HAUL_LAT) / 2
  noaa_data$CENTROID_LON <- (noaa_data$BEGIN_HAUL_LON + noaa_data$END_HAUL_LON) / 2
}

# Determine the length of a haul in km
noaa_data$HAUL_LENGTH_KM <- mapply(function(lat1, lon1, lat2, lon2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000 # Convert meters to kilometers
}, noaa_data$BEGIN_HAUL_LAT, noaa_data$BEGIN_HAUL_LON, noaa_data$END_HAUL_LAT, noaa_data$END_HAUL_LON)

# Adding lunar calculations
{
  noaa_data$MOON_DISTANCE = moonAngle(noaa_data$MID_HAUL_UTC, longitude=noaa_data$CENTROID_LON, latitude = noaa_data$CENTROID_LAT)$distance  
  noaa_data$MOON_ILLUMINATED =  moonAngle(noaa_data$MID_HAUL_UTC, longitude=noaa_data$CENTROID_LON, latitude = noaa_data$CENTROID_LAT)$illuminatedFraction
  noaa_data$MOON_PHASE_RADIANS = lunar.phase(as.POSIXct(noaa_data$MID_HAUL_DATE_TIME))
  noaa_data$MOON_PHASE = lunar.phase(as.POSIXct(noaa_data$MID_HAUL_DATE_TIME), name = 8)
}

# Add effort ID and days report
noaa_data <- noaa_data %>%
  filter(SPECIES_NAME != "NOCATCH" & !is.na(BEGIN_SET_DATE_TIME) & !is.na(CENTROID_LAT)& !is.na(CENTROID_LON)) %>%
  mutate(Effort_ID = dense_rank(TRIP_NUMBER)) %>%
  group_by(TRIP_NUMBER) %>%
  mutate(Days_Report = dense_rank(as.Date(BEGIN_SET_DATE_TIME, format="%m/%d/%Y"))) %>%
  ungroup()

# Top species caught overall
top <- noaa_data %>%
  group_by(SPECIES_NAME) %>%
  summarise(TOTAL = sum(NUM_FISH)) %>%
  arrange(desc(TOTAL))
top_species <- names(sort(tapply(noaa_data$NUM_FISH, noaa_data$SPECIES_NAME, sum), decreasing = TRUE))[1:5]

# Group by year and species to get total catches per year
bar <- noaa_data %>%
  group_by(SET_YEAR, SPECIES_NAME) %>%
  summarise(Total_Sp_Yr = sum(NUM_FISH), .groups = "drop") %>%
  arrange(desc(Total_Sp_Yr))

# Modify data to group non-top species as "Other"
bar_modified <- bar %>%
  mutate(SPECIES_NAME = if_else(SPECIES_NAME %in% top_species_all_years, as.character(SPECIES_NAME), "OTHER")) %>%
  group_by(SET_YEAR, SPECIES_NAME) %>%  # Group by year and species (including "Other")
  summarise(Total_Sp_Yr = sum(Total_Sp_Yr, na.rm = TRUE), .groups = "drop") %>%  # Sum counts for "Other"
  mutate(SPECIES_NAME = factor(SPECIES_NAME, levels = c("OTHER", top_species_all_years)))

# Define custom colors
custom_colors <- c("#2c3e50", "#95a5a6", "#18bc9c", "#3498db", "#f39c12", "#e74c3c")

# bar chart plotly
bar_plotly <- plot_ly(
  data = bar_modified,
  x = ~SET_YEAR,
  y = ~Total_Sp_Yr,
  color = ~SPECIES_NAME,
  colors = custom_colors,
  type = 'bar'
) %>%
  layout(
    xaxis = list(title = "", tickformat = "%Y"),
    yaxis = list(title = "Total Fish Caught", tickformat = ",d"),
    barmode = "stack",
    legend = list(title = list(text = "Species Name"), orientation = "h", x = 0, y = -0.2)
  )

observer_trips <- noaa_data %>%
  group_by(SET_YEAR) %>%
  summarize(TRIPS = n_distinct(TRIP_NUMBER))

sum(observer_trips$TRIPS)

# trips plotly
trips_plotly <- plot_ly(observer_trips) %>%
  add_lines(
    x = ~SET_YEAR, y = ~TRIPS,
    color = I("#2c3e50")
  ) %>%
  layout(
    xaxis = list(visible = F, showgrid = F, title = ""),
    yaxis = list(visible = F, showgrid = F, title = list(text="Observed Trips", standoff=20L)),
    hovermode = "x",
    margin = list(t = 0, r = 0, l = 0, b = 0),
    font = list(color = "#2c3e50"),
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent"
  ) %>%
  config(displayModeBar = F) %>%
  htmlwidgets::onRender(
    "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen, 'yaxis.visible': ev.detail.fullScreen});
        })
    }"
  )

# Read shapefiles
gridshp10 <- st_read(dsn="HMS_App/shapefiles", layer = "GOM_GRID_10MIN_smooth")
gridshp10=gridshp10[1]
names(gridshp10)[names(gridshp10) == "FID_1"] <- "GRID_ID"
invalid_cells <- gridshp10[!st_is_valid(gridshp10), ]
gridshp10 <- gridshp10 %>% filter(!GRID_ID %in% invalid_cells$GRID_ID)

gridshp15 <- st_read(dsn="HMS_App/shapefiles", layer = "GOM_GRID_15MIN_smooth")
gridshp15=gridshp15[1]
names(gridshp15)[names(gridshp15) == "FID_1"] <- "GRID_ID"

gridshp10h <- st_read(dsn="HMS_App/shapefiles", layer = "GOM_HEX_10MIN")
gridshp10h=gridshp10h[3]
names(gridshp10h)[names(gridshp10h) == "GRID_ID"] <- "GRID_ID"

boat_icon <- makeIcon(iconUrl = "www/boat2.svg",
                      iconWidth=35, iconHeight=30, 
                      iconAnchorX=15, iconAnchorY=15)
html_legend <- "<img src='boat2.svg' style='width:35px;height:30px;'> Current Location<br/>"
}

# Save variables for use in the server
save(bar_plotly, trips_plotly, 
     gridshp10, gridshp15, gridshp10h, 
     top_species, noaa_data, 
     boat_icon, html_legend, file = "HMS_App/preprocess.RData")

# check for invalid geometries in shapefil
# ggplot() +
#   geom_sf(data = gridshp10, fill = NA, color = "red") +
#   geom_sf(data = gridshp10[!st_is_valid(gridshp10),], fill = "blue")

# non-reactive gam
# gam_data <- noaa_data %>% filter(SPECIES_NAME %in% c("SWORDFISH", "ESCOLAR", "TUNA ALBACORE", 
#                                          "TUNAS", "TUNA BIGEYE", "TUNA BLACKFIN", "TUNA YELLOWFIN", "DOLPHIN FISH (MAHI MAHI)")) %>%
#   group_by(UNIQUE_RETRIEVAL, SPECIES_NAME) %>%
#   mutate(
#     NUM_FISH = NUM_FISH,
#     NUM_DAM = sum(NUM_FISH[CONDITION %in% c('DAMAGED')]),
#     PROP_DAM = NUM_DAM / NUM_FISH,
#     FISH_PER_KM = NUM_FISH / HAUL_LENGTH_KM,
#     FISH_PER_10KM = FISH_PER_KM * 10,
#     SOAK_TIME = as.numeric(difftime(BEGIN_HAUL_DATE_TIME, END_SET_DATE_TIME, units = "hours")),
#     FISH_PER_SOAK_HR = NUM_FISH / SOAK_TIME,
#     FISH_PER_8SOAK_HR = FISH_PER_SOAK_HR * 8,
#     UNIQUE_RET_LAT = round(mean(CENTROID_LAT), 6),
#     UNIQUE_RET_LON = round(mean(CENTROID_LON), 6),
#     MID_HAUL_UTC = MID_HAUL_UTC,
#     MID_HAUL_NUMERIC = as.numeric(MID_HAUL_UTC)
#   )
# 
# # Fit a GAM model
# gam_model <- gam(FISH_PER_10KM ~ s(MID_HAUL_NUMERIC, bs = "cs"), data = gam_data)  # "cs" is cubic spline
# 
# # Predict fitted values
# df_pred <- gam_data %>%
#   ungroup() %>%
#   mutate(y_pred = predict(gam_model, newdata = gam_data, type = "response"))
# 
# # Plot raw data and GAM smooth in Plotly
# plot_ly() %>%
#   add_markers(data = gam_data, x = ~MID_HAUL_UTC, y = ~FISH_PER_10KM, name = "Observed Data", marker = list(opacity = 0.5)) %>%
#   add_lines(data = df_pred, x = ~MID_HAUL_UTC, y = ~y_pred, name = "GAM Fit", line = list(width = 2)) %>%
#   layout(
#     title = "GAM Fit with Plotly",
#     xaxis = list(title = "MID_HAUL_UTC", tickformat="%Y-%m", type="date", tickmode="auto", nticks=16),
#     yaxis = list(title = "FISH_PER_10KM"),
#     legend = list(orientation = "h", x = 0, y = -0.2)
#   )
