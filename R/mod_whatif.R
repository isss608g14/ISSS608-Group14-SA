#' whatif Module
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
#' @import tidyr
#' @import tibble
#' @import stringr
#' @import glmnet
#' @import broom


mod_whatif_ui <- function(id) {
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
        .switch-container {
          display: flex;
          flex-direction: row;
          flex-wrap: wrap;
          justify-content: center;
          gap: 2rem 3rem;
          margin-bottom: 1rem;
        }
        .switch-box {
          display: inline-flex;
          flex-direction: column;
          align-items: center;
          min-width: 100px;
        }
      "))
    ),
    div(
      class = "light-theme",
      h2("🎯 Breakout Simulation: What If?"),
      p("Select artists and toggle breakout traits to simulate a score change."),
      
      # Artist selectors and switches
      fluidRow(
        column(6,
               div(
                 style = "display: flex; justify-content: center;",
                 div(
                   style = "width: 60%;",
                   shinyWidgets::pickerInput(
                     inputId = ns("artist1"),
                     label = "Artist 1",
                     choices = NULL,
                     options = list(`actions-box` = TRUE, `live-search` = TRUE, `container` = 'body')
                   )
                 )
               ),
               div(class = "switch-container", uiOutput(ns("switches1")))
        ),
        column(6,
               div(
                 style = "display: flex; justify-content: center;",
                 div(
                   style = "width: 60%;",
                   shinyWidgets::pickerInput(
                     inputId = ns("artist2"),
                     label = "Artist 2",
                     choices = NULL,
                     options = list(`actions-box` = TRUE, `live-search` = TRUE, `container` = 'body')
                   )
                 )
               ),
               div(class = "switch-container", uiOutput(ns("switches2")))
        )
      ),
      
      br(),
      
      # Probability text and charts
      fluidRow(
        column(6, htmlOutput(ns("prob_text1"), class = "text-center")),
        column(6, htmlOutput(ns("prob_text2"), class = "text-center"))
      ),
      fluidRow(
        column(6, plotly::plotlyOutput(ns("race_chart1"), height = "320px")),
        column(6, plotly::plotlyOutput(ns("race_chart2"), height = "320px"))
      )
    )
  )
}



mod_whatif_server <- function(id, nodes_tbl, edges_tbl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load RDA data
    load("data/nodes_tbl.rda")  # loads nodes_tbl
    load("data/edges_tbl.rda")  # loads edges_tbl
    
    features <- c("score_performer", "score_cluster", "score_multirole",
                  "score_crossover", "score_label", "score_sampled")
    
    trait_weights <- c(
      performer = 0.17,
      cluster = 0.17,
      multirole = 0.17,
      crossover = 0.16,
      label = 0.16,
      sampled = 0.17
    )
    
    # Initialize artist dropdowns
    artist_names <- nodes_tbl %>%
      dplyr::filter(node_type %in% c("Person", "MusicalGroup")) %>%
      dplyr::arrange(name) %>%
      dplyr::pull(name)
    
    observe({
      shinyWidgets::updatePickerInput(
        session,
        "artist1",
        choices = artist_names,
        selected = if ("Sailor Shift" %in% artist_names) "Sailor Shift" else artist_names[1]
      )
      shinyWidgets::updatePickerInput(
        session,
        "artist2",
        choices = artist_names,
        selected = artist_names[2]
      )
    })
    
    # UI for switches
    create_switch_ui <- function(id_prefix) {
      lapply(features, function(feat) {
        div(class = "switch-box",
            shinyWidgets::prettySwitch(
              inputId = ns(paste0(id_prefix, "_", feat)),
              label = gsub("score_", "", feat),
              value = FALSE,
              status = "success",
              slim = TRUE,
              inline = TRUE
            )
        )
      })
    }
    
    output$switches1 <- renderUI({ create_switch_ui("a1") })
    output$switches2 <- renderUI({ create_switch_ui("a2") })
    
    # Helper: actual scores
    get_actual_scores <- function(artist_name) {
      artist_id <- nodes_tbl %>% filter(name == artist_name) %>% pull(id)
      if (length(artist_id) == 0) return(NULL)
      
      id_val <- artist_id[1]
      scores <- list(
        score_performer = as.integer(id_val %in% (edges_tbl %>% filter(edge_type == "PerformerOf") %>% pull(source))),
        score_cluster = 0,
        score_multirole = as.integer(id_val %in% (edges_tbl %>% filter(edge_type %in% c("ComposerOf", "LyricistOf", "ProducerOf")) %>% pull(source))),
        score_crossover = as.integer(id_val %in% (edges_tbl %>% filter(edge_type %in% c("InStyleOf", "InterpolatesFrom", "CoverOf")) %>% pull(source))),
        score_label = as.integer(id_val %in% (edges_tbl %>% filter(edge_type %in% c("RecordedBy", "DistributedBy")) %>% pull(source))),
        score_sampled = as.integer(id_val %in% (edges_tbl %>% filter(edge_type %in% c("CoverOf", "LyricalReferenceTo", "DirectlySamples")) %>% pull(target)))
      )
      return(scores)
    }
    
    compute_probability <- function(score_vector) {
      round(sum(score_vector) / length(score_vector), 2)
    }
    
    # Combine current and simulated scores
    get_race_chart_data <- function(artist_name, switches_prefix) {
      scores <- get_actual_scores(artist_name)
      if (is.null(scores)) return(NULL)
      
      trait_names <- gsub("score_", "", features)
      
      current_vec <- unlist(scores[features])
      simulated_vec <- mapply(function(f, actual) {
        if (actual == 1) return(1)
        as.numeric(input[[paste0(switches_prefix, "_", f)]])
      }, features, current_vec)
      
      df <- tibble::tibble(
        Trait = trait_names,
        Current = current_vec,
        Simulated = simulated_vec
      )
      
      df$Prob_Current <- compute_probability(current_vec)
      df$Prob_Simulated <- compute_probability(simulated_vec)
      
      return(df)
    }
    
    
    # Render plot
    render_race_chart <- function(df, title_text) {
      plot_ly(df, y = ~Trait, x = ~Current, type = "bar", orientation = 'h',
              name = "Current", marker = list(color = "green")) %>%
        add_trace(
          data = df[df$Simulated != df$Current, ],
          x = ~Simulated, y = ~Trait,
          type = "bar", orientation = "h",
          name = "Simulated",
          marker = list(color = "grey")
        ) %>%
        layout(
          title = list(text = title_text, x = 0),
          barmode = "overlay",
          xaxis = list(
            range = c(0, 1.2),
            title = "",
            showticklabels = FALSE,
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(title = ""),
          legend = list(orientation = "h", x = 0.4, y = -0.2),
          paper_bgcolor = "#f9f7f1",
          plot_bgcolor = "#f9f7f1",
          font = list(color = "#2f2f2f")
        )
    }
    
    output$race_chart1 <- plotly::renderPlotly({
      df <- get_race_chart_data(input$artist1, "a1")
      if (!is.null(df)) render_race_chart(df, paste("🎯", input$artist1, "Score Simulation"))
    })
    
    output$race_chart2 <- plotly::renderPlotly({
      df <- get_race_chart_data(input$artist2, "a2")
      if (!is.null(df)) render_race_chart(df, paste("🎯", input$artist2, "Score Simulation"))
    })
    
    output$prob_text1 <- renderUI({
      df <- get_race_chart_data(input$artist1, "a1")
      if (is.null(df)) return("")
      
      prob_curr <- round(unique(df$Prob_Current) * 100)
      prob_sim <- round(unique(df$Prob_Simulated) * 100)
      
      color <- if (prob_sim >= 80) {
        "#2ecc71"  # green
      } else if (prob_sim >= 50) {
        "#f39c12"  # orange
      } else {
        "#e74c3c"  # red
      }
      
      HTML(paste0("<h4 style='color:", color, "'>💡 Probability Score: ",
                  prob_sim, "% <small style='color:#2f2f2f;'>(was ", prob_curr, "%)</small></h4>"))
    })
    
    output$prob_text2 <- renderUI({
      df <- get_race_chart_data(input$artist2, "a2")
      if (is.null(df)) return("")
      
      prob_curr <- round(unique(df$Prob_Current) * 100)
      prob_sim <- round(unique(df$Prob_Simulated) * 100)
      
      color <- if (prob_sim >= 80) {
        "#2ecc71"
      } else if (prob_sim >= 50) {
        "#f39c12"
      } else {
        "#e74c3c"
      }
      
      HTML(paste0("<h4 style='color:", color, "'>💡 Probability Score: ",
                  prob_sim, "% <small style='color:#2f2f2f;'>(was ", prob_curr, "%)</small></h4>"))
    })
  })
}
