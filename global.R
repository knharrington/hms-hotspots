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

  #library(rasterVis)
  #library(raster)
  #library(RColorBrewer)

  library(leaflet)
  library(kableExtra)
  library(shiny)
  library(plotly)
  library(htmlwidgets)
  library(shinyWidgets)
  #library(shinythemes)
  library(bslib)
  #library(shinyBS)
  library(shinycustomloader)
  library(leaflet.extras)
  library(leaflet.extras2)
  
  #library(KernSmooth)
  #library(ks)
  
  #library(geosphere)
  #library(lunar)
  #library(suncalc)
  #library(oce)
}

# Load data made in the preprocessing file
load("preprocess.RData")


