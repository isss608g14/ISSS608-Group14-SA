#' feature Module
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

mod_feature_ui <- function(id) {
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
      h2("🔍 Feature Breakdown by Artist"),
      
      div(
        style = "text-align: center;",
        div(
          style = "display: inline-block; min-width: 300px; margin-right: 20px;",
          shinyWidgets::pickerInput(
            inputId = ns("genre_filter"),
            label = "Select Genre(s)",
            choices = NULL,
            options = list(`actions-box` = TRUE, `live-search` = TRUE, `container` = 'body'),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block; min-width: 300px;",
          sliderInput(
            inputId = ns("top_n"),
            label = "Top N Artists by Total Score",
            min = 10, max = 30, value = 20, step = 5
          )
        )
      ),
      
      br(),
      
      plotly::plotlyOutput(ns("feature_heatmap"), height = "300px") %>%
        shinycssloaders::withSpinner(),
      
      br(),
      
      DT::dataTableOutput(ns("feature_table")) %>%
        shinycssloaders::withSpinner()
    )
  )
}



mod_feature_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load RDA data
    load("data/nodes_tbl.rda")  # loads nodes_tbl
    load("data/edges_tbl.rda")  # loads edges_tbl
    
    # Populate genre choices
    observe({
      genres <- sort(unique(unlist(strsplit(nodes_tbl$genre[!is.na(nodes_tbl$genre)], ";"))))
      updatePickerInput(session, "genre_filter", choices = genres, selected = "Oceanus Folk")
    })
    
    artist_scores <- reactive({
      req(input$genre_filter)
      
      # Step 1: Recent notable songs filtered by genre
      recent_songs <- nodes_tbl %>%
        dplyr::filter(
          node_type == "Song",
          !is.na(genre),
          stringr::str_detect(genre, paste(input$genre_filter, collapse = "|")),
          notable == TRUE,
          as.integer(release_date) >= 2030
        ) %>%
        dplyr::select(song_id = id, release_date)
      
      # Trait 1
      score_performer <- edges_tbl %>%
        dplyr::filter(edge_type == "PerformerOf") %>%
        dplyr::rename(artist_id = source, song_id = target) %>%
        dplyr::inner_join(recent_songs, by = "song_id") %>%
        dplyr::count(artist_id) %>%
        dplyr::filter(n >= 3) %>%
        dplyr::mutate(score_performer = 1) %>%
        dplyr::select(artist_id, score_performer)
      
      # Trait 2
      cluster_window <- edges_tbl %>%
        dplyr::filter(edge_type == "PerformerOf") %>%
        dplyr::rename(artist_id = source, song_id = target) %>%
        dplyr::inner_join(recent_songs, by = "song_id") %>%
        dplyr::mutate(release_date = as.integer(release_date)) %>%
        dplyr::group_by(artist_id) %>%
        dplyr::summarise(
          span = max(release_date) - min(release_date),
          recent_max = max(release_date),
          song_count = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::filter(song_count >= 2, span <= 3, recent_max >= 2030) %>%
        dplyr::mutate(score_cluster = 1) %>%
        dplyr::select(artist_id, score_cluster)
      
      # Trait 3
      recent_multi_roles <- edges_tbl %>%
        dplyr::filter(edge_type %in% c("ComposerOf", "LyricistOf", "ProducerOf")) %>%
        dplyr::inner_join(nodes_tbl %>% dplyr::select(id, release_date), by = c("target" = "id")) %>%
        dplyr::filter(as.integer(release_date) >= 2030) %>%
        dplyr::count(source) %>%
        dplyr::rename(artist_id = source) %>%
        dplyr::mutate(score_multirole = 1) %>%
        dplyr::select(artist_id, score_multirole)
      
      # Trait 4
      cross_genre_edges <- edges_tbl %>%
        dplyr::filter(edge_type %in% c("InStyleOf", "InterpolatesFrom", "CoverOf")) %>%
        dplyr::inner_join(nodes_tbl %>% dplyr::select(id, genre, release_date), by = c("target" = "id")) %>%
        dplyr::filter(!is.na(genre), genre != "Oceanus Folk", as.integer(release_date) >= 2030) %>%
        dplyr::count(source) %>%
        dplyr::rename(artist_id = source) %>%
        dplyr::mutate(score_crossover = 1) %>%
        dplyr::select(artist_id, score_crossover)
      
      # Trait 5
      recent_label_links <- edges_tbl %>%
        dplyr::filter(edge_type %in% c("RecordedBy", "DistributedBy")) %>%
        dplyr::inner_join(nodes_tbl %>% dplyr::select(id, release_date), by = c("source" = "id")) %>%
        dplyr::filter(as.integer(release_date) >= 2030) %>%
        dplyr::count(source) %>%
        dplyr::rename(artist_id = source) %>%
        dplyr::mutate(score_label = 1) %>%
        dplyr::select(artist_id, score_label)
      
      # Trait 6
      recent_referenced <- edges_tbl %>%
        dplyr::filter(edge_type %in% c("CoverOf", "LyricalReferenceTo", "DirectlySamples")) %>%
        dplyr::inner_join(nodes_tbl %>% dplyr::select(id, release_date), by = c("target" = "id")) %>%
        dplyr::filter(as.integer(release_date) >= 2030) %>%
        dplyr::count(target) %>%
        dplyr::rename(artist_id = target) %>%
        dplyr::mutate(score_sampled = 1) %>%
        dplyr::select(artist_id, score_sampled)
      
      # Combine all
      artist_features <- score_performer %>%
        dplyr::full_join(cluster_window, by = "artist_id") %>%
        dplyr::full_join(recent_multi_roles, by = "artist_id") %>%
        dplyr::full_join(cross_genre_edges, by = "artist_id") %>%
        dplyr::full_join(recent_label_links, by = "artist_id") %>%
        dplyr::full_join(recent_referenced, by = "artist_id") %>%
        tidyr::replace_na(list(
          score_performer = 0, score_cluster = 0, score_multirole = 0,
          score_crossover = 0, score_label = 0, score_sampled = 0
        ))
      
      artist_names <- nodes_tbl %>%
        dplyr::filter(node_type %in% c("Person", "MusicalGroup")) %>%
        dplyr::select(artist_id = id, name)
      
      artist_features %>%
        dplyr::inner_join(artist_names, by = "artist_id") %>%
        tidyr::pivot_longer(
          cols = dplyr::starts_with("score_"),
          names_to = "feature",
          values_to = "score"
        )
    })
    
    output$feature_heatmap <- plotly::renderPlotly({
      df_all <- artist_scores()
      req(nrow(df_all) > 0)
      
      # Calculate total score and keep Top N
      top_n <- input$top_n
      top_artists <- df_all %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(total = sum(score), .groups = "drop") %>%
        dplyr::arrange(desc(total)) %>%
        head(top_n)
      
      df <- df_all %>%
        dplyr::inner_join(top_artists, by = "name")
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = feature, y = reorder(name, total), fill = factor(score))) +
        ggplot2::geom_tile(color = "grey70") +
        ggplot2::scale_fill_manual(values = c("0" = "#dddddd", "1" = "#2a5a5b")) +
        ggplot2::labs(x = "Feature", y = "Artist", fill = "Score") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 10),
          plot.background = ggplot2::element_rect(fill = "#f9f7f1"),
          panel.background = ggplot2::element_rect(fill = "#f9f7f1"),
          text = ggplot2::element_text(color = "#2f2f2f"),
          axis.text = ggplot2::element_text(color = "#2f2f2f"),
          legend.background = ggplot2::element_rect(fill = "#f9f7f1"),
          legend.text = ggplot2::element_text(color = "#2f2f2f"),
          legend.title = ggplot2::element_text(color = "#2f2f2f")
        )
      
      plotly::ggplotly(p, tooltip = c("y", "x", "fill"))
    })
    
    output$feature_table <- DT::renderDataTable({
      df_all <- artist_scores()
      req(nrow(df_all) > 0)
      
      top_n <- input$top_n
      top_artists <- df_all %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(total_score = sum(score), .groups = "drop") %>%
        dplyr::arrange(desc(total_score)) %>%
        head(top_n)
      
      df_all %>%
        dplyr::inner_join(top_artists, by = "name") %>%
        tidyr::pivot_wider(names_from = feature, values_from = score) %>%
        dplyr::select(name, total_score,
                      score_performer, score_cluster, score_multirole,
                      score_crossover, score_label, score_sampled) %>%
        DT::datatable(
          escape = FALSE,
          rownames = FALSE,
          options = list(
            pageLength = 5,
            dom = 'p',
            order = list(list(1, 'desc')),
            columnDefs = list(list(className = 'dt-left', targets = "_all"))
          )
        )
    })
    
  })
}