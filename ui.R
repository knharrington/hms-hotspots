################################################################################
# This script builds the user interface for the HMS app
# capabilities include: viewing maps and customization options
################################################################################

cards <- list(
  card(
    full_screen = TRUE,
    card_header("Map"),
    card_body(leafletOutput("examplemap"))  #height = "67vh"
  ), # map card
  card(
    full_screen = TRUE,
    card_header("Top Species Catch Events"),
    card_body(plotlyOutput("topspeciesbar"))
  ), # bar graph card
  card(
    full_screen = TRUE,
    card_header("CPUE Over Time"),
    card_body(plotlyOutput("species_gam"))
  ) # gam graph card
)

page_sidebar(title = "Bycatch Hotspots Initiative: Highly Migratory Species", 
   theme = bs_theme(preset="flatly"),          
   #class = "bslib-page-dashboard",          
   sidebar = sidebar(width=400,
                     helpText("Use the following selections to update the data displayed on the map."),
                     pickerInput("species", "Species", 
                                 choices = list(
                                   `Target Species` = c("SWORDFISH", "ESCOLAR", "TUNA ALBACORE", "TUNAS", "TUNA BIGEYE", 
                                                        "TUNA BLACKFIN", "TUNA YELLOWFIN", "DOLPHIN FISH (MAHI MAHI)"),
                                   `Bycatch Species` = c("ALL SHARKS", "TUNA BLUEFIN"),
                                   `Other Species` = c("BILLFISH", "MARLIN BLUE", "MARLIN WHITE", 
                                                       "LANCETFISH", "WHITE MARLIN/ROUNDSCALE SPEAR", "SAILFISH")),
                                 multiple=TRUE, options = pickerOptions(container = "body", actionsBox = TRUE), width = "100%"
                                 ),
                     sliderTextInput("days", "Time range: days since reporting", 
                                     choices=seq(from=max(noaa_data$Days_Report), to=min(noaa_data$Days_Report), by=-1), 
                                     selected=c(max(noaa_data$Days_Report), min(noaa_data$Days_Report)), grid=TRUE),
                     checkboxGroupInput("lunar", "Lunar phase", 
                                        choices = c("New", "Waxing crescent", "First quarter", "Waxing gibbous", "Full", 
                                                    "Waning gibbous", "Last quarter", "Waning crescent"),
                                        selected = c("New", "Waxing crescent", "First quarter", "Waxing gibbous", "Full", 
                                                     "Waning gibbous", "Last quarter", "Waning crescent")),
                     radioGroupButtons("radio_grid", "Grid shape", 
                                 choices = c("10 Min Square", "15 Min Square", "10 Min Hexagon"),
                                 selected = "10 Min Square"
                                 ),
                     awesomeRadio("radio_layer", "Layer type",
                                  choices = c("Catch per 10 km", "Catch per 8 soak hrs", "Depredation Intensity", "Catch Density"),
                                  selected = "Catch per 10 km"
                                  ),
                     actionButton("update", "Update Map", icon=icon("refresh"), class="btn btn-primary", style = "color: white;")
        ), #sidebar
   
   layout_columns(
     fill = FALSE,
     value_box(
       title = "Total Observations",
       value = textOutput("text_obs"),
       showcase = bsicons::bs_icon("binoculars")
     ),
     value_box(
       title = "Observed Trips",
       value = 118,
       theme = "text-blue",
       showcase = trips_plotly,
       showcase_layout = "bottom",
       full_screen = TRUE
     )
   ),
        
   layout_columns(
     col_widths = c(8,4),
     cards[[1]], 
     layout_columns(
       cards[[2]],
       cards[[3]],
       col_widths=c(12,12)
     )
   )
   
) # sidebar page
  
