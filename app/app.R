#------------------------------------------------------------------------------#
# Map of Medway - Map App
# Author: Sam Finnegan
# Date: 20-04-2026
#------------------------------------------------------------------------------#

# Clean up and load libraries --------------------------------------------------
rm(list=ls())
library(tidyverse)
library(janitor)
library(leaflet)
library(sf)
library(qs2)
library(shiny)
library(shinyWidgets)
library(bslib)

# Set options and read in data -------------------------------------------------
options(shiny.autoreload = TRUE)
qs_readm("./data/MapData.qs2")

# UI ---------------------------------------------------------------------------

ui <- page_sidebar(
  
  # Add style sheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  ),
  
  
  # Set window title
  window_title = "Medway Map",
  
  # Define what goes in the sidebar
  sidebar = sidebar(
    width = 275,
    radioGroupButtons(inputId = "select_AreaLevel",
                      label = "Select area level:",
                      choices = c("Wards", "LSOA"))
  ),
  
  
  # Main body
  div(class = "header-block",
      h2("MEDWAY GREEN PARTY LOCAL ELECTION MAP")),
  card(id = "map_card",
    card_body(
      leafletOutput(outputId = "Map", height = 600)
    ),
    full_screen = TRUE
  )
  
)


# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  output$Map <- renderLeaflet({
    
    if (input$select_AreaLevel == "Wards") {
      
      leaflet(Wards) |>
        addProviderTiles("CartoDB", group = "CartoDB") |>
        addPolygons(weight = 1,
                    color = "#ffffff",
                    fillColor = "#00643b",
                    fillOpacity = 0.7)
      
    } else if (input$select_AreaLevel == "LSOA") {
      
      leaflet(LSOA) |>
        addProviderTiles("CartoDB", group = "CartoDB") |>
        addPolygons(weight = 1,
                    color = "#ffffff",
                    fillColor = "#00643b",
                    fillOpacity = 0.7)
      
    } else {}
    
  })
  
}

# RUN APP ----------------------------------------------------------------------

shinyApp(ui, server)