#' star Module
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

mod_star_ui <- function(id) {
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
        .light-theme th,
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
      h2("🌟 Rising Stars Leaderboard: Scoring & Predictions", style = "color: #2f2f2f;"),
      br(),
      tags$strong("Post-LASSO Linear Model:", style = "color: #2f2f2f;"),
      div(
        tags$p("After validating all six features using LASSO regression, resulting formula is:")
      ),
      div(
        style = "margin-bottom: 20px;",
        tags$em("Total Score = score_performer + score_cluster + score_multirole + score_crossover + score_label + score_sampled")
      ),
      tags$u("Scoring Logic Table", style = "color: #2f2f2f;"),
      DT::dataTableOutput(ns("scoring_table")),
      br(),
      fluidRow(
        column(
          6,
          shinyWidgets::pickerInput(
            inputId = ns("genre_filter"),
            label = "Select Genres:",
            choices = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `container` = 'body')
          )
        )
      ),
      DT::dataTableOutput(ns("leaderboard"))
    )
  )
}


mod_star_server <- function(input, output, session) {
  ns <- session$ns
  
  output$scoring_table <- DT::renderDataTable({
    scoring_table <- tibble::tibble(
      No = c(
        "1", 
        "2", 
        "3", 
        "4", 
        "5", 
        "6"
      ),
      Traits = c(
        "Performer Role", 
        "Clustered Releases", 
        "Multi-Role", 
        "Cross-Genre Influence", 
        "Label Signed", 
        "Sampled/Referenced"
      ),
      Component = c(
        "score_performer", 
        "score_cluster", 
        "score_multirole", 
        "score_crossover", 
        "score_label", 
        "score_sampled"
      ),
      Score = c(
        "+1",
        "+1",
        "+1",
        "+1",
        "+1",
        "+1"
      ),
      Criteria = c(
        "Artist performed ≥3 notable songs released between 2030–2035",
        "At least 2 notable songs released within a 2–3 year period, most recent in 2033–2035",
        "Artist took on new roles (Composer, Lyricist, Producer) on songs from 2030–2035",
        "Artist's recent songs (2030–2035) have influence edges to other genres via InStyleOf, CoverOf, InterpolatesFrom",
        "Artist had RecordedBy or DistributedBy label connections on works from 2030–2035",
        "Artist’s work was covered, sampled, or referenced (e.g., CoverOf, LyricalReferenceTo) from 2030–2035"
      ),
      Why = c(
        "Indicates sustained momentum and creative output just before the forecast window",
        "Suggests a current creative spike with room to grow in the next 5 years",
        "Shows deepening industry involvement and potential to shape future sound",
        "Indicates artistic flexibility and broadening appeal",
        "Signals formal industry backing just prior to breakout",
        "Reflects increasing peer recognition, a classic pre-stardom signal"
      )
    )
    
    DT::datatable(
      scoring_table,
      rownames = FALSE,
      options = list(
        dom = 't',
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ),
      class = 'compact stripe hover'
    )
  })
  
  load("data/nodes_tbl.rda")  
  
  observe({
    genres <- sort(unique(unlist(strsplit(nodes_tbl$genre, ";"))))
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "genre_filter",
      choices = genres,
      selected = "Oceanus Folk",
    )
  })
  
  filtered_scores <- reactive({
    req(input$genre_filter)
    
    ## Trait 1: Performer Role w/ ≥3 Notable Songs    
    recent_selected_songs <- nodes_tbl %>%
      dplyr::filter(
        node_type == "Song",
        !is.na(genre),
        stringr::str_detect(genre, paste(input$genre_filter, collapse = "|")),
        notable == TRUE,
        as.integer(release_date) >= 2030
      ) %>%
      dplyr::select(song_id = id, release_date)
    
    recent_performances <- edges_tbl %>%
      dplyr::filter(edge_type == "PerformerOf") %>%
      dplyr::rename(artist_id = source, song_id = target) %>%
      dplyr::inner_join(recent_selected_songs, by = "song_id")
    
    score_performer <- recent_performances %>%
      dplyr::count(artist_id, name = "recent_notable_songs") %>%
      dplyr::filter(recent_notable_songs >= 3) %>%
      dplyr::mutate(score_performer = 1)
    
    ## Trait 2: Tightly Clustered Releases    
    cluster_window <- recent_performances %>%
      dplyr::mutate(release_date = as.integer(release_date)) %>%
      dplyr::group_by(artist_id) %>%
      dplyr::summarise(
        span = max(release_date) - min(release_date),
        recent_max = max(release_date),
        song_count = dplyr::n()
      ) %>%
      dplyr::filter(song_count >= 2, span <= 3, recent_max >= 2030) %>%
      dplyr::mutate(score_cluster = 1)
    
    ## Trait 3: Multi-Role
    recent_multi_roles <- edges_tbl %>%
      dplyr::filter(edge_type %in% c("ComposerOf", "LyricistOf", "ProducerOf")) %>%
      dplyr::inner_join(nodes_tbl %>% dplyr::select(id, release_date), by = c("target" = "id")) %>%
      dplyr::filter(as.integer(release_date) >= 2030) %>%
      dplyr::count(source, name = "multi_role_count") %>%
      dplyr::rename(artist_id = source) %>%
      dplyr::mutate(score_multirole = 1)    
    
    ## Trait 4: Cross-Genre Influence
    cross_genre_edges <- edges_tbl %>%
      dplyr::filter(edge_type %in% c("InStyleOf", "InterpolatesFrom", "CoverOf")) %>%
      dplyr::inner_join(nodes_tbl %>% dplyr::select(id, genre, release_date), by = c("target" = "id")) %>%
      dplyr::filter(!is.na(genre), genre != "Oceanus Folk", as.integer(release_date) >= 2030) %>%
      dplyr::count(source, name = "cross_genre_count") %>%
      dplyr::rename(artist_id = source) %>%
      dplyr::mutate(score_crossover = 1)
    
    ## Trait 5: Label
    recent_label_links <- edges_tbl %>%
      dplyr::filter(edge_type %in% c("RecordedBy", "DistributedBy")) %>%
      dplyr::inner_join(nodes_tbl %>% dplyr::select(id, release_date), by = c("source" = "id")) %>%
      dplyr::filter(as.integer(release_date) >= 2030) %>%
      dplyr::count(source, name = "label_links") %>%
      dplyr::rename(artist_id = source) %>%
      dplyr::mutate(score_label = 1)
    
    ## Trait 6: Sampled
    recent_referenced <- edges_tbl %>%
      dplyr::filter(edge_type %in% c("CoverOf", "LyricalReferenceTo", "DirectlySamples")) %>%
      dplyr::inner_join(nodes_tbl %>% dplyr::select(id, release_date), by = c("target" = "id")) %>%
      dplyr::filter(as.integer(release_date) >= 2030) %>%
      dplyr::count(target, name = "referenced_count") %>%
      dplyr::rename(artist_id = target) %>%
      dplyr::mutate(score_sampled = 1)
    
    ## Combine scores
    final_scores_recent <- score_performer %>%
      dplyr::full_join(cluster_window, by = "artist_id") %>%
      dplyr::full_join(recent_multi_roles, by = "artist_id") %>%
      dplyr::full_join(cross_genre_edges, by = "artist_id") %>%
      dplyr::full_join(recent_label_links, by = "artist_id") %>%
      dplyr::full_join(recent_referenced, by = "artist_id") %>%
      tidyr::replace_na(list(
        score_performer = 0,
        score_cluster = 0,
        score_multirole = 0,
        score_crossover = 0,
        score_label = 0,
        score_sampled = 0
      )) %>%
      dplyr::mutate(total_score = score_performer + score_cluster + score_multirole +
                      score_crossover + score_label + score_sampled)
    
    artist_names <- nodes_tbl %>%
      dplyr::filter(node_type %in% c("Person", "MusicalGroup")) %>%
      dplyr::select(artist_id = id, name)
    
    final_scores_recent <- final_scores_recent %>%
      dplyr::inner_join(artist_names, by = "artist_id")
  })    
  
  # Leaderboard  
  output$leaderboard <- DT::renderDataTable({
    filtered_scores() %>%
      dplyr::arrange(desc(total_score)) %>%
      dplyr::select(name, total_score, score_performer, score_cluster, score_multirole,
                    score_crossover, score_label, score_sampled) %>%
      DT::datatable(
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = 'p',
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        )
      )
  })
}
