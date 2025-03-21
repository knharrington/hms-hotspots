################################################################################
# This script is the global environment for the HMS app
# It stores objects needed in the sever and/or ui

################################################################################

# load packages
{
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(mgcv)
  
  library(sp)
  library(sf)

  library(leaflet)
  library(kableExtra)
  library(shiny)
  library(plotly)
  library(htmlwidgets)
  library(shinyWidgets)
  library(shinycustomloader)
  library(leaflet.extras)
  library(leaflet.extras2)
  
}

# Load data made in the preprocessing file
load("preprocess.RData")

# Set Google sheet information
googledrive::drive_auth(cache = ".secrets", email = "knharrington@mote.org")
googlesheets4::gs4_auth(token = drive_token())

# Define water current data
water_currents <- "https://geo.gcoos.org/data/hycom/hycom_surface_current.json"
