#' trend Module
#'
#' @description A shiny Module for Rising Stars Dashboard.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import shinyWidgets
#' @import DT
#' @import ggplot2
#' @import dplyr
#' @import tidyverse
#' @import tidyr
#' @import tibble
#' @import stringr
#' @import janitor
#' @import stringr
#' @import plotly
#' @import golem



  mod_trend_ui <- function(id) {
    ns <- NS(id)
    tagList(
      tags$head(
        tags$style(HTML("
          .light-theme {
            background-color: #f9f7f1;
            color: #2f2f2f;
            padding: 1rem;
            border-radius: 12px;
          }
          .light-theme .form-control,
          .light-theme .selectpicker {
            color: #2f2f2f !important;
            background-color: #ffffff !important;
          }
        "))
      ),
      div(
        class = "light-theme",
        fluidPage(
          titlePanel(div("🎶 Genre Influence Over Time", style = "color: #2f2f2f;")),
          sidebarLayout(
            sidebarPanel(
              shinyWidgets::pickerInput(
                inputId = ns("genre_filter"),
                label = "Select Genres:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `container` = 'body'
                )
              ),
              sliderInput(ns("yearRange"), "Year Range:",
                          min = 1990, max = 2040, value = c(2010, 2030)),
              checkboxGroupInput(
                ns("edgeFilter"),
                "Edge Types:",
                choices = c("InspiredBy", "InterpolatesFrom", "InStyleOf"),
                selected = c("InspiredBy", "InterpolatesFrom", "InStyleOf")
              ),
              checkboxInput(ns("showTrend"), "Show Trendline", TRUE)
            ),
            mainPanel(
              shinycssloaders::withSpinner(plotly::plotlyOutput(ns("linePlot"), height = "300px")),
              br(),
              h4("Detailed Influence Edges", style = "color: #2f2f2f;"),
              shinycssloaders::withSpinner(DT::DTOutput(ns("edgeTable")))
            )
          )
        )
      )
    )
  }
  
  
  mod_trend_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      # Load RDA data
      load("data/nodes_tbl.rda")  # loads nodes_tbl
      load("data/edges_tbl.rda")  # loads edges_tbl
      
      # Genre dropdown init
      observe({
        genres <- sort(unique(unlist(strsplit(nodes_tbl$genre, ";"))))
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "genre_filter",
          choices = genres,
          selected = "Oceanus Folk",
        )
      })
      
      # Main influence logic
      result_df <- reactive({
        req(input$genre_filter, input$yearRange, input$edgeFilter)
        
        genre_songs <- nodes_tbl %>%
          filter(node_type == "Song", genre == input$genre_filter, !is.na(release_date)) %>%
          mutate(release_year = as.integer(floor(as.numeric(release_date))))
        
        infl_edges <- edges_tbl %>%
          filter(edge_type %in% input$edgeFilter, source %in% genre_songs$id) %>%
          transmute(song_id = source, infl_song = target, rel_type = edge_type) %>%
          left_join(
            genre_songs %>% transmute(song_id = id, year = release_year),
            by = "song_id"
          ) %>%
          left_join(
            edges_tbl %>% filter(edge_type == "PerformerOf") %>%
              transmute(infl_song = target, performer = source),
            by = "infl_song"
          ) %>%
          left_join(
            nodes_tbl %>% filter(node_type == "Person") %>%
              transmute(performer = id, influencer = name),
            by = "performer"
          ) %>%
          filter(!is.na(influencer), !is.na(year),
                 year >= input$yearRange[1], year <= input$yearRange[2])
        
        yearly_counts <- infl_edges %>%
          count(year, name = "num_influences") %>%
          right_join(tibble(year = seq(input$yearRange[1], input$yearRange[2])), by = "year") %>%
          replace_na(list(num_influences = 0)) %>%
          arrange(year)
        
        list(summary = yearly_counts, details = infl_edges)
      })
      
      # Plot
      output$linePlot <- plotly::renderPlotly({
        df <- result_df()$summary
        
        p <- plot_ly(
          data = df,
          x = ~year,
          y = ~num_influences,
          type = "scatter",
          mode = "lines+markers",
          name = "Influences",
          line = list(shape = "spline"),
          marker = list(size = 8)
        )
        
        # Add trend line if selected
        if (input$showTrend && nrow(df) >= 2) {
          lm_fit <- lm(num_influences ~ year, data = df)
          df$trend <- predict(lm_fit, newdata = df)
          
          p <- p %>%
            add_lines(
              data = df,
              x = ~year,
              y = ~trend,
              name = "Trend Line",
              line = list(dash = "dash", color = "red")
            )
        }
        
        p %>% layout(
          title = list(
            text = paste("Influence Trend -", input$genre_filter),
            font = list(family = "Playfair Display", color = "#2f2f2f", size = 17)
          ),
          xaxis = list(
            title = list(text = "Year", font = list(family = "Playfair Display", color = "#2f2f2f"))
          ),
          yaxis = list(
            title = list(text = "Number of Influences", font = list(family = "Playfair Display", color = "#2f2f2f"))
          ),
          font = list(family = "Playfair Display", color = "#2f2f2f"),
          plot_bgcolor = "#f9f7f1",
          paper_bgcolor = "#f9f7f1"
        )
      })
      
      
      # Table
      output$edgeTable <- DT::renderDT({
        result_df()$details %>%
          select(year, rel_type, influencer, infl_song) %>%
          arrange(desc(year)) %>%
          DT::datatable(
            rownames = FALSE,
            filter = 'top',
            extensions = c('Buttons'),
            options = list(
              pageLength = 5,
              autoWidth = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf'),
              columnDefs = list(list(className = 'dt-center', targets = "_all"))
            ),
            class = "stripe hover row-border nowrap"
          )
      })
    })
  }
  
