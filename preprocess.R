# preprocessing for the HMS app

# load packages
{
  library(tidyverse)
  library(data.table)
  library(lubridate)
  
  library(sp)
  library(sf)

  library(geosphere)
  library(lunar)
  library(suncalc)
  library(oce)
  
  library(googledrive)
  library(googlesheets4)
  library(digest)
}

# Set global options for handling Google Sheets token
options(
  gargle_oauth_email=TRUE,
  gargle_oauth_cache = ".secrets",
  gargle_oauth_path = NULL
)

# Assign Google Sheet ID
sheet_id <- drive_get("HMS-sheet")$id

{
# Read data
Data.In <- fread("data/NOAA-HMS-data.csv")
noaa_data = Data.In

# Function to generate random alphanumeric string of the same length
random_string <- function(n) {
  paste(sample(c(0:9, letters, LETTERS), n, replace = TRUE), collapse = "")
}

# Create a function to map unique values to random strings
random_mask <- function(column) {
  # Get unique values in the column
  unique_values <- unique(column)
  
  # Generate random strings for each unique value, with the same length as the original value
  masked_values <- sapply(nchar(unique_values), random_string)
  
  # Create a mapping table (dictionary)
  value_map <- setNames(masked_values, unique_values)
  
  # Replace the original values with their masked versions
  return(value_map[column])
}

# Mask columns with sensitive information
{
  noaa_data$VESSEL_NAME <- random_mask(noaa_data$VESSEL_NAME)
  noaa_data$VESSEL_ID <- noaa_data$VESSEL_NAME
  noaa_data$TRIP_NUMBER <- random_mask(noaa_data$TRIP_NUMBER)
  noaa_data$UNIQUE_RETRIEVAL <- paste(noaa_data$VESSEL_ID, noaa_data$TRIPNUMBER, noaa_data$SETNUMBER, sep="_")
}

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
  #noaa_data$UNIQUE_RETRIEVAL <- paste(noaa_data$VESSEL_ID, noaa_data$TRIP_NUMBER, noaa_data$HAUL_NUMBER, sep="_")
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

gridshp <- st_read(dsn="shapefiles", layer = "GOM_GRID_15MIN_smooth")
gridshp=gridshp[1]
names(gridshp)[names(gridshp) == "FID_1"] <- "GRID_ID"

boat_icon <- makeIcon(iconUrl = "www/boat.svg",
                      iconWidth=35, iconHeight=30, 
                      iconAnchorX=15, iconAnchorY=15)
html_legend <- "<img src='boat.svg' style='width:35px;height:30px;'> Current Location<br/>"
}

# create an example for a fixed userbase and hash the passwords
user_base <- tibble::tibble(
  user = c("cfemm-admin", "test-user"),
  password = purrr::map_chr(c("temp", "hotspots"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("Admin Account", "Test User")
)

# Save variables for use in the server
save(sheet_id, user_base, gridshp, noaa_data, 
     boat_icon, html_legend, file = "preprocess.RData")

################################################################################
