#' artist Module
#'
#' @description A shiny Module for Rising Stars Dashboard.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import shinydashboard
#' @import igraph
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import lubridate
#' @import plotly
#' @import DT
#' @import shinycssloaders
#' @import shinyWidgets


mod_artist_ui <- function(id) {
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

        .light-theme .dataTables_wrapper {
          color: #2f2f2f;
        }

        .light-theme table {
          color: #2f2f2f !important;
          background-color: #ffffff !important;
        }

        .light-theme th {
          color: #2f2f2f !important;
        }

        .light-theme td {
          color: #2f2f2f !important;
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
        titlePanel(
          div("🎵 Artist Influence Over Time", style = "color: #2f2f2f;")
        ),
        sidebarLayout(
          sidebarPanel(
            shinyWidgets::pickerInput(
              inputId = ns("artist1"),
              label = "Choose Artist(s):",
              choices = NULL,  
              selected = NULL,  
              multiple = TRUE,
              options = list(
                `live-search` = TRUE,
                `size` = 7,
                `container` = 'body'
              )
            ),
            sliderInput(ns("yearRange"), "Year Range:",
                        min = 1990, max = 2040,
                        value = c(2010, 2040), step = 1, sep = "")
          ),
          mainPanel(
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns("linePlot"))),
            br(),
            h4("Detailed Influence Edges", style = "color: #2f2f2f;"),
            shinycssloaders::withSpinner(DT::DTOutput(ns("edgeTable")))
          )
        )
      )
    )
  )
}




mod_artist_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load RDA data
    load("data/nodes_tbl.rda")  # loads nodes_tbl
    load("data/edges_tbl.rda")  # loads edges_tbl
    
    wanted_types <- c(
      "InStyleOf", "LyricalReferenceTo", "InterpolatesFrom", "DirectlySamples",
      "CoverOf", "Remixes", "References", "Mentions", "SamplesFrom",
      "InspiredBy", "Recreates", "AdaptedFrom"
    )
    
    # Influence function - now takes only passed-in nodes_tbl and edges_tbl
    get_influence_over_time <- function(
    artist_name,
    yearRange = c(2010, 2040),
    influence_types = wanted_types,
    nodes_tbl,
    edges_tbl
    ) {
      artist_id <- nodes_tbl %>%
        dplyr::filter(node_type == "Person", name == artist_name) %>%
        dplyr::pull(id)
      
      if (length(artist_id) == 0) stop("Artist not found")
      
      artist_songs <- edges_tbl %>%
        dplyr::filter(edge_type == "PerformerOf", source == artist_id) %>%
        dplyr::pull(target)
      
      infl_edges <- edges_tbl %>%
        dplyr::filter(edge_type %in% influence_types, source %in% artist_songs) %>%
        dplyr::transmute(song_id = source, infl_song = target, rel_type = edge_type) %>%
        dplyr::left_join(
          nodes_tbl %>% dplyr::filter(node_type == "Song") %>%
            dplyr::transmute(song_id = id, year = as.integer(substr(release_date, 1, 4))),
          by = "song_id"
        ) %>%
        dplyr::left_join(
          edges_tbl %>% dplyr::filter(edge_type == "PerformerOf") %>%
            dplyr::transmute(infl_song = target, performer = source),
          by = "infl_song"
        ) %>%
        dplyr::left_join(
          nodes_tbl %>% dplyr::filter(node_type == "Person") %>%
            dplyr::transmute(performer = id, influencer = name),
          by = "performer"
        ) %>%
        dplyr::filter(!is.na(influencer), !is.na(year),
                      year >= yearRange[1], year <= yearRange[2])
      
      yearly_counts <- infl_edges %>%
        dplyr::count(year, name = "num_influences") %>%
        dplyr::right_join(tibble::tibble(year = seq(yearRange[1], yearRange[2])), by = "year") %>%
        tidyr::replace_na(list(num_influences = 0)) %>%
        dplyr::arrange(year)
      
      list(summary = yearly_counts, details = infl_edges)
    }
    
    # Populate artist list
    observe({
      req(nodes_tbl)
      artist_names <- sort(unique(nodes_tbl$name[nodes_tbl$node_type == "Person"]))
      
      default_artists <- c("Sailor Shift")
      default_selection <- default_artists[default_artists %in% artist_names]  
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "artist1",
        choices = artist_names,
        selected = default_selection
      )
    })
    
    # Reactive filtered influence data
    result_df <- reactive({
      req(input$artist1, input$yearRange)
      get_influence_over_time(
        artist_name = input$artist1,
        yearRange = input$yearRange,
        nodes_tbl = nodes_tbl,
        edges_tbl = edges_tbl
      )
    })
    
    # Trend plot
    output$linePlot <- plotly::renderPlotly({
      req(input$artist1, input$yearRange)
      
      base_data <- lapply(input$artist1, function(artist) {
        df <- get_influence_over_time(artist, input$yearRange, nodes_tbl = nodes_tbl, edges_tbl = edges_tbl)$summary
        df$artist <- artist
        df
      }) %>% dplyr::bind_rows()
      
      all_years <- sort(unique(base_data$year))
      cumulative_data <- lapply(all_years, function(y) {
        base_data %>% dplyr::filter(year <= y) %>% dplyr::mutate(frame = y)
      }) %>% dplyr::bind_rows()
      
      avg_selected <- mean(base_data$num_influences, na.rm = TRUE)
      min_year <- min(all_years)
      max_year <- max(all_years)
      
      plotly::plot_ly(
        data = cumulative_data,
        x = ~year,
        y = ~num_influences,
        frame = ~frame,
        color = ~artist,
        colors = c("#0B2447", "#1F6E8C", "#88CCF1", "#A5D7E8", "#4FBDBA"),
        type = "scatter",
        mode = "lines+markers",
        line = list(shape = "spline", width = 4),
        marker = list(size = 9, line = list(color = "#FFFFFF", width = 1.5)),
        text = ~paste("Artist:", artist, "<br>Year:", year, "<br>Influences:", num_influences),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          title = list(text = "Influence Trend Over Time", font = list(size = 24), x = 0),
          xaxis = list(title = "", tickformat = "d", showgrid = FALSE),
          yaxis = list(title = "Number of Influences", showgrid = TRUE,
                       gridcolor = 'rgba(180,180,180,0.2)', zeroline = FALSE),
          font = list(family = "Helvetica Neue", size = 14, color = "#0B2447"),
          margin = list(t = 80, b = 60, l = 70, r = 30),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.2),
          paper_bgcolor = "#f9f7f1",   
          plot_bgcolor = "#f9f7f1",    
          annotations = list(
            list(
              text = "Use the scrubber below to view the trend year by year",
              showarrow = FALSE,
              xref = "paper", yref = "paper",
              x = 0, y = -0.25,
              font = list(size = 12, color = "#1F6E8C")
            )
          )
        ) %>%
        plotly::animation_opts(frame = 600, transition = 0, redraw = TRUE)
    })
    
    # Detailed table
    output$edgeTable <- DT::renderDT({
      result_df()$details %>%
        dplyr::select(year, rel_type, influencer, infl_song) %>%
        dplyr::arrange(desc(year)) %>%
        DT::datatable(
          rownames = FALSE,
          filter = 'top',
          extensions = c('Buttons'),
          options = list(
            pageLength = 3,
            autoWidth = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
          ),
          class = "stripe hover row-border nowrap"
        )
    })
  })
}