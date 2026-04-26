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
library(shinyjs)
library(bslib)
library(reactable)
library(reactablefmtr)

# Set options and read in data -------------------------------------------------
options(shiny.autoreload = TRUE)
qs_readm("./data/appData.qs2")

# UI ---------------------------------------------------------------------------

ui <- page_fillable(
  
  useShinyjs(),
  
  # Add style sheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  ),
  
  sidebar = NULL,
  
  # Main body
  # Map
  div(class = "header-block",
      style = "display: flex; flex-direction: column;",
      h2("MEDWAY GREEN PARTY LOCAL ELECTION MAP")),
  card(id = "map_card",
       height = "500px",
       fill = FALSE,
       card_body(
         leafletOutput(outputId = "Map", height = 500)
       )
  ),
  
  div(class = "stats",
      style = "display: flex; flex-direction: column;",
      uiOutput("StatsUI"))
)


# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Render the map
  output$Map <- renderLeaflet({
    
    bbox <- sf::st_bbox(Wards)
    
    leaflet(Wards,
            options = leafletOptions(
              zoomControl      = FALSE,
              dragging         = FALSE,
              scrollWheelZoom  = FALSE,
              doubleClickZoom  = FALSE,
              boxZoom          = FALSE,
              keyboard         = FALSE)) |>
      addProviderTiles("CartoDB", group = "CartoDB") |>
      addPolygons(layerId = ~wd25nm,
                  label = ~wd25nm,
                  labelOptions = labelOptions(textsize = "16px",
                                              style = list("font-family" = 'Manrope')),
                  weight = 1,
                  color = "#ffffff",
                  fillColor = "#00643b",
                  fillOpacity = 0.7) |>
      setView(
        lng  = (bbox[["xmin"]] + bbox[["xmax"]]) / 2,  # center of bounding box
        lat  = (bbox[["ymin"]] + bbox[["ymax"]]) / 2,
        zoom = 11  # adjust this
      )
    
    
  })
  
  # Set up a reactive value to store which ward has been clicked
  clickedWard <- reactiveVal(NULL)
  
  
  # observeEvent for map clip
  observeEvent(input$Map_shape_click, {
    
    # Update the reactive value to store ward id
    click <- input$Map_shape_click
    clickedWard(click$id)
    
    # Show radioGroupButton for party/councillors - hidden by default
    shinyjs::show(id = "select_view_past")
    shinyjs::show(id = "select_view_future")
    
    # leafletProxy updates map styling to show which is clicked
    leafletProxy("Map", session) |>
      
      # Reset all wards to default style
      addPolygons(data = Wards,
                  layerId = ~wd25nm,
                  label = ~wd25nm,
                  labelOptions = labelOptions(textsize = "16px",
                                              style = list("font-family" = 'Manrope')),
                  weight = 1,
                  color = "#ffffff",
                  fillColor = "#00643b",
                  fillOpacity = 0.7) |>
      
      # Highlight the clicked ward
      addPolygons(data = Wards |> filter(wd25nm == click$id),
                  layerId = ~wd25nm,
                  label = ~wd25nm,
                  labelOptions = labelOptions(textsize = "16px",
                                              style = list("font-family" = 'Manrope')),
                  weight = 2,
                  color = "#ffffff",
                  fillColor = "#00a85a",
                  fillOpacity = 0.9)
    
  })
  
  
  # Put everything else in a renderUI and don't show unless !is.null(clickedWard())
  output$StatsUI <- renderUI({
    
    if (is.null(clickedWard())) {
      
      card(
        card_body(
          "Click a ward on the map to display data"
        )
      )
      
    } else {
      
      div(
        id = "flex-column",
        style = "display: flex; flex-direction: column",
        
        # Quick stat cards
        layout_column_wrap(
          width = 1/4,
          
          div(class = "stat1",
              style = "margin-bottom: 0 !important;",
              card(
                card_header("Ward"),
                card_body(textOutput("Ward"))
              )
          ),
          
          div(div = "stat2",
              style = "margin-bottom: 0 !important;",
              card(
                card_header("Councillors"),
                card_body(textOutput("NumCouncillors"))
              )
          ),
          
          div(class = "stat3",
              style = "margin-bottom: 0 !important;",
              card(
                card_header("Electors (2023)"),
                card_body(textOutput("Electors"))
              )
          ),
          
          div(div = "stat4",
              style = "margin-bottom: 0 !important;",
              card(
                card_header("Turnout (2023)"),
                card_body(textOutput("Turnout"))
              )
          )
        ),
        
        # Members per population value card
        div(
          class = "residentspermember",
          value_box(
            title = "Residents per member",
            value = textOutput("ResidentsPerMemberValue"),
            showcase = span(
              style = "color: #00643b;",
              bsicons::bs_icon("bullseye")
            ),
            showcase_layout = "left center"
            
          )
        ),
        
        
        # Canvassing results value card
        div(
          class = "canvassing score",
          value_box(
            title = html("Canvassing score - How likely are you to vote Green? <br> 1 = Definitely, 5 = Definitely not"),
            value = textOutput("CanvassingScore"),
            showcase = span(
              style = "color: #00643b;",
              bsicons::bs_icon("chat")
            ),
            showcase_layout = "left center"
            
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
                     radioGroupButtons(inputId = "select_view_past",
                                       label = "",
                                       choices = c("PARTY", "COUNCILLORS"),
                                       selected = "PARTY"),
                     uiOutput(outputId = "PastTable")
                   ),
              )
          ),
          
          # Projected results
          div(class = "table2",
              card(id = "projected_result_26",
                   card_header("Projected Result (2026)"),
                   card_body(
                     height = 400,
                     radioGroupButtons(inputId = "select_view_future",
                                       label = "",
                                       choices = c("PARTY", "COUNCILLORS"),
                                       selected = "PARTY"),
                     uiOutput(outputId = "FutureTable")
                   )
              )
          )
          
        ),
        
        
        # ProjectionsSF
        div(class = "samprojections",
            card(id = "projected_result_26_sf",
                 card_header("Projected Result (2026, population-based)"),
                 card_body(
                   height = 400,
                   htmlOutput("SFProjectionsExplainer"),
                   reactableOutput("SFProjectionsTable")
                 )
            )
        )
      )
      
    }
    
  })
  
  
  # Ward name
  output$Ward <- renderText({
    req(!is.null(clickedWard()))
    clickedWard()
  })
  
  # Number of councillors
  output$NumCouncillors <- renderText({
    req(!is.null(clickedWard()))
    Election23Results |>
      filter(wd25nm == clickedWard()) |>
      select(number_councillors) |>
      pull() |>
      unique() |>
      as.character()
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
      select(turnout, electors) |>
      unique() |>
      mutate(turnout_percent = turnout/electors) |>
      select(turnout_percent) |>
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
      select(councillors, party, votes, vote_percent, colour, text_colour) |>
      arrange(desc(vote_percent))
    
  })
  
  
  # Election result table in a renderUI
  output$PastTable <- renderUI({
    validate(
      need(!is.null(clickedWard()), "Click a ward to display data")
    )
    
    if (input$select_view_past == "PARTY") {
      
      reactableOutput(outputId = "Election23TableParty")
      
      
    } else if (input$select_view_past == "COUNCILLORS") {
      
      reactableOutput(outputId = "Election23TableCouncillors")
      
    } else {
      
    }
    
  })
  
  
  # Election 2023 results table - By party
  output$Election23TableParty <- renderReactable({
    
    validate(
      need(!is.null(clickedWard()), "Click a ward to display data")
    )
    
    df <- PastResults() |>
      group_by(party, colour, text_colour) |>
      summarise(votes = sum(votes)) |>
      ungroup() |>
      mutate(vote_percent = votes/sum(votes))
    
    
    reactable(df,
              style = list(fontFamily = "Manrope", fontSize = "1rem"),
              highlight = TRUE,
              borderless = TRUE,
              defaultPageSize = nrow(df),
              defaultSorted = list(vote_percent = "desc"),
              columns = list(
                party = colDef(name = "PARTY",
                               minWidth = 120,
                               maxWidth = 300),
                votes = colDef(name = "VOTES",
                               minWidth = 70,
                               maxWidth = 100),
                vote_percent = colDef(name = "VOTE SHARE",
                                      minWidth = 100,
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
  
  # Election 2023 results table - By councillors
  output$Election23TableCouncillors <- renderReactable({
    
    df <- PastResults()
    
    
    reactable(df,
              style = list(fontFamily = "Manrope", fontSize = "1rem"),
              highlight = TRUE,
              borderless = TRUE,
              defaultPageSize = nrow(df),
              defaultSorted = list(vote_percent = "desc"),
              columns = list(
                councillors = colDef(name = "councillors",
                                     minWidth = 110,
                                     maxWidth = 300),
                party = colDef(name = "PARTY",
                               minWidth = 120,
                               maxWidth = 300),
                votes = colDef(name = "VOTES",
                               minWidth = 70,
                               maxWidth = 100),
                vote_percent = colDef(name = "VOTE SHARE",
                                      minWidth = 100,
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
      select(councillors,
             party,
             projected_votes_number_votes_cast_2023,
             projected_vote_percent,
             percent_projected_vote_share_per_party,
             projected_vote_share_per_party,
             colour,
             text_colour) |>
      arrange(desc(projected_vote_percent))
    
  })
  
  
  # Election result table in a renderUI
  output$FutureTable <- renderUI({
    validate(
      need(!is.null(clickedWard()), "Click a ward to display data")
    )
    
    if (input$select_view_future == "PARTY") {
      
      reactableOutput(outputId = "ProjectedTableParty")
      
      
    } else if (input$select_view_future == "COUNCILLORS") {
      
      reactableOutput(outputId = "ProjectedTableCouncillors")
      
    } else {
      
    }
    
  })
  
  # Projected election results table - party
  output$ProjectedTableParty <- renderReactable({
    
    validate(
      need(!is.null(clickedWard()), "Click a ward to display data")
    )
    
    df <- FutureResults() |>
      select(party,
             projected_vote_share_per_party,
             percent_projected_vote_share_per_party,
             colour,
             text_colour) |>
      group_by(party, colour, text_colour) |>
      summarise(projected_vote_share_per_party = sum(projected_vote_share_per_party),
                percent_projected_vote_share_per_party = sum(percent_projected_vote_share_per_party)) |>
      ungroup()
    
    
    reactable(df,
              style = list(fontFamily = "Manrope", fontSize = "1rem"),
              highlight = TRUE,
              borderless = TRUE,
              defaultPageSize = nrow(df),
              defaultSorted = list(percent_projected_vote_share_per_party = "desc"),
              columns = list(
                party = colDef(name = "PARTY",
                               minWidth = 120,
                               maxWidth = 300),
                projected_vote_share_per_party = colDef(name = "VOTES",
                                                        minWidth = 70,
                                                        maxWidth = 100,
                                                        format = colFormat(digits = 0)),
                percent_projected_vote_share_per_party = colDef(name = "VOTE SHARE",
                                                                minWidth = 100,
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
  
  # Projected election results table - councillors
  output$ProjectedTableCouncillors <- renderReactable({
    
    validate(
      need(!is.null(clickedWard()), "Click a ward to display data")
    )
    
    df <- FutureResults() |>
      select(councillors,
             party,
             projected_votes_number_votes_cast_2023,
             projected_vote_percent,
             colour,
             text_colour)
    
    
    reactable(df,
              style = list(fontFamily = "Manrope", fontSize = "1rem"),
              highlight = TRUE,
              borderless = TRUE,
              defaultPageSize = nrow(df),
              defaultSorted = list(projected_vote_percent = "desc"),
              columns = list(
                councillors = colDef(name = "CANDIDATE",
                                     minWidth = 110,
                                     maxWidth = 300),
                party = colDef(name = "PARTY",
                               minWidth = 120,
                               maxWidth = 300),
                projected_votes_number_votes_cast_2023 = colDef(name = "VOTES",
                                                                minWidth = 70,
                                                                maxWidth = 100,
                                                                format = colFormat(digits = 0)),
                projected_vote_percent = colDef(name = "VOTE SHARE",
                                                minWidth = 100,
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
  
  
  # Data for residents per member value_box
  output$ResidentsPerMemberValue <- renderText({
    
    M2W |>
      filter(wd25nm == clickedWard()) |>
      pull(rank_word)
    
  })
  
  
  # Data for canvassing value_box
  output$CanvassingScore <- renderText({
    
    Score <- Canvas |>
      filter(wd25nm == clickedWard()) |>
      pull(revised_average)
    
    Text <- case_when(is.na(Score) ~ "No data yet",
                      TRUE ~ paste0("Average score: ", round(Score, 2)))
    
  })
  
  
  # Sam Projections Reactable
  output$SFProjectionsTable <- renderReactable({
    
    df <- ProjectionSF |>
      filter(wd25nm == clickedWard())
    
    reactable(df,
              style = list(fontFamily = "Manrope", fontSize = "1rem"),
              highlight = TRUE,
              borderless = TRUE,
              defaultPageSize = nrow(df),
              defaultSorted = list(votes_percent = "desc"),
              columns = list(
                party = colDef(name = "PARTY",
                               minWidth = 120,
                               maxWidth = 300),
                votes = colDef(name = "VOTES",
                               minWidth = 70,
                               maxWidth = 100,
                               format = colFormat(digits = 0)),
                votes_percent = colDef(name = "VOTE SHARE",
                                       minWidth = 100,
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
                text_colour = colDef(show = FALSE),
                wd25nm = colDef(show = FALSE))
    )
    
  })
  
  
  # Sam projections table explainer
  output$SFProjectionsExplainer <- renderText({
    
    paste0(
      "<p>This projection uses population estimates for Medway, estimates of 2023 voter",
      " turnout from the <a href = 'https://www.britishelectionstudy.com/data-object/wave-25-of-the-2014-2023-british-election-study-internet-panel/' target = '_blank'>British Election Study<a/>,",
      " and voting intention as of January 2026 from <a href = 'https://yougov.com/en-gb/articles/53923-how-would-britain-vote-at-the-start-of-2026' target = '_blank'>YouGov</a>. ",
      " These data are split by gender and age. We have then assumed that people turn out as they did in 2023 and vote in line with YouGov's voting intention for their age and gender.</p>"
    )
    
  })
  
  
}

# RUN APP ----------------------------------------------------------------------

shinyApp(ui, server)