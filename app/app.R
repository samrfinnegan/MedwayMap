#------------------------------------------------------------------------------#
# Map of Medway - Map App
# Author: Sam Finnegan
# Date: 20-04-2026
#------------------------------------------------------------------------------#

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(leaflet)
library(sf)
library(qs2)
library(shiny)
library(shinyWidgets)
library(bslib)
library(reactable)
library(reactablefmtr)

# Set options and read in data -------------------------------------------------
options(shiny.autoreload = TRUE)
qs_readm("./data/MapData.qs2")
qs_readm("./data/Election23Results.qs2")

# UI ---------------------------------------------------------------------------

ui <- page_fillable(
  
  # Add style sheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  ),
  
  
  # Set window title
  window_title = "Medway Map",
  
  # Define what goes in the sidebar
  # sidebar = sidebar(
  #   width = 350,
  #   radioGroupButtons(inputId = "select_MapView",
  #                     label = "Select view:",
  #                     choices = c("2023 Results", "Projection"))
  # ),
  sidebar = NULL,
  
  # Main body
  # Map
  div(class = "header-block",
      h2("MEDWAY GREEN PARTY LOCAL ELECTION MAP")),
  card(id = "map_card",
       card_body(
         leafletOutput(outputId = "Map", height = 600)
       ),
       full_screen = TRUE
  ),
  
  
  # Quick stat cards
  layout_column_wrap(
    width = 1/3,
    
    div(class = "stat1",
        card(
          card_header("Ward"),
          card_body(textOutput("Ward"))
        )
    ),
    
    div(class = "stat2",
        card(
          card_header("Electors"),
          card_body(textOutput("Electors (2023)"))
        )
    ),
    
    div(div = "stats3",
        card(
          card_header("Turnout"),
          card_body(textOutput("Turnout (2023)"))
        )
    )
  ),
  
  
  # Results table cards
  layout_column_wrap(
    width = 1/2,
    
    # Election 2023 results
    div(class = "table1",
        card(id = "election_result_23",
             card_header("Election Results (2023)"),
             card_body(
               height = 400,
               reactableOutput(outputId = "Election23Table")
             ),
        )
    ),
    
    # Projected results
    div(class = "table2",
        card(id = "projected_result_26",
             card_header("Projected Result (2026)"),
             card_body(
               height = 400,
               reactableOutput(outputId = "ProjectedTable")
             )
        )
    )
    
    
  )
  
)


# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Render the map
  output$Map <- renderLeaflet({
    
    
    leaflet(Wards) |>
      addProviderTiles("CartoDB", group = "CartoDB") |>
      addPolygons(layerId = ~wd25nm,
                  label = ~wd25nm,
                  weight = 1,
                  color = "#ffffff",
                  fillColor = "#00643b",
                  fillOpacity = 0.7)
    
    
  })
  
  # Set up a reactive value to store which ward has been clicked
  clickedWard <- reactiveVal(NULL)
  
  
  # Update the clickedWard and show the clicked ward on the map
  observeEvent(input$Map_shape_click, {
    click <- input$Map_shape_click
    clickedWard(click$id)
    
    leafletProxy("Map", session) |>
      
      # Reset all wards to default style
      addPolygons(data = Wards,
                  layerId = ~wd25nm,
                  label = ~wd25nm,
                  weight = 1,
                  color = "#ffffff",
                  fillColor = "#00643b",
                  fillOpacity = 0.7) |>
      
      # Highlight the clicked ward
      addPolygons(data = Wards |> filter(wd25nm == click$id),
                  layerId = ~wd25nm,
                  label = ~wd25nm,
                  weight = 2,
                  color = "#ffffff",
                  fillColor = "#00a85a",
                  fillOpacity = 0.9)
    
  })
  
  
  # Ward name
  output$Ward <- renderText({
    req(!is.null(clickedWard()))
    clickedWard()
  })
  
  # Electors
  output$Electors <- renderText({
    req(!is.null(clickedWard()))
    Election23Results |>
      filter(wd25nm == clickedWard()) |>
      select(electors) |>
      pull() |>
      unique() |>
      as.numeric() |>
      format(big.mark = ",")
  })
  
  #Turnout
  output$Turnout <- renderText({
    req(!is.null(clickedWard()))
    Percentage <- Election23Results |>
      filter(wd25nm == clickedWard()) |>
      select(turnout) |>
      unique() |>
      as.numeric()
    
    Percentage <- round(Percentage*100, 1)
    
    Percentage <- paste0(Percentage, "%")
    
    return(Percentage)
    
  })
  
  
  # Elections 2023 reactive filter
  PastResults <- reactive({
    
    req(!is.null(clickedWard()))
    
    Election23Results |>
      filter(wd25nm == clickedWard()) |>
      select(candidate, party, votes, percent_of_electors, colour, text_colour) |>
      arrange(desc(percent_of_electors))
    
  })
  
  
  # Election 2023 results table
  output$Election23Table <- renderReactable({
    
    validate(
      need(!is.null(clickedWard()), "Click a ward to display data")
    )
    
    df <- PastResults()
    
    
    reactable(df,
              style = list(fontFamily = "Manrope", fontSize = "1rem"),
              highlight = TRUE,
              borderless = TRUE,
              defaultPageSize = nrow(df),
              defaultSorted = list(percent_of_electors = "desc"),
              columns = list(
                candidate = colDef(name = "CANDIDATE",
                                   minWidth = 100,
                                   maxWidth = 300),
                party = colDef(name = "PARTY",
                               minWidth = 100,
                               maxWidth = 300),
                votes = colDef(name = "VOTES",
                               minWidth = 50,
                               maxWidth = 100),
                percent_of_electors = colDef(name = "SHARE OF ELECTORATE",
                                             minWidth = 200,
                                             cell = data_bars(df,
                                                              text_color_ref = "text_colour",
                                                              brighten_text = FALSE,
                                                              text_position = "inside-base",
                                                              bold_text = TRUE,
                                                              max_value = 1,
                                                              bar_height = 25,
                                                              background = "#ffffff",
                                                              fill_color_ref = "colour",
                                                              align_bars = "left",
                                                              number_fmt = scales::percent_format(accuracy = 0.1))),
                colour = colDef(show = FALSE),
                text_colour = colDef(show = FALSE))
    )
    
  })
  
  # Projected 2026 reactive filter
  FutureResults <- reactive({
    
    req(!is.null(clickedWard()))
    
    Election23Results |>
      filter(wd25nm == clickedWard()) |>
      select(candidate, party, votes_2, percent_of_electorate, colour, text_colour) |>
      arrange(desc(percent_of_electorate))
    
  })
  
  
  # Projected election results table
  output$ProjectedTable <- renderReactable({
    
    validate(
      need(!is.null(clickedWard()), "Click a ward to display data")
    )
    
    df <- FutureResults()
    
    
    reactable(df,
              style = list(fontFamily = "Manrope", fontSize = "1rem"),
              highlight = TRUE,
              borderless = TRUE,
              defaultPageSize = nrow(df),
              defaultSorted = list(percent_of_electorate = "desc"),
              columns = list(
                candidate = colDef(name = "CANDIDATE",
                                   minWidth = 100,
                                   maxWidth = 300),
                party = colDef(name = "PARTY",
                               minWidth = 100,
                               maxWidth = 300),
                votes_2 = colDef(name = "VOTES",
                                 minWidth = 50,
                                 maxWidth = 100,
                                 format = colFormat(digits = 0)),
                percent_of_electorate = colDef(name = "SHARE OF ELECTORATE",
                                               minWidth = 200,
                                               cell = data_bars(df,
                                                                text_color_ref = "text_colour",
                                                                brighten_text = FALSE,
                                                                text_position = "inside-base",
                                                                bold_text = TRUE,
                                                                max_value = 1,
                                                                bar_height = 25,
                                                                background = "#ffffff",
                                                                fill_color_ref = "colour",
                                                                align_bars = "left",
                                                                number_fmt = scales::percent_format(accuracy = 0.1))),
                colour = colDef(show = FALSE),
                text_colour = colDef(show = FALSE))
    )
    
  })
  
  
}

# RUN APP ----------------------------------------------------------------------

shinyApp(ui, server)