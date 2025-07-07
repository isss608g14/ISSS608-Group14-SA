#' network Module
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
#' @import visNetwork
#' @importFrom visNetwork visNetworkOutput renderVisNetwork visNetwork

mod_network_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .light-theme {
          background-color: #2a5a5b;
          color: #2f2f2f;
          padding: 1rem;
          border-radius: 12px;
        }
      "))
    ),
    div(class = "light-theme",
        h2("🌐 Artist Influence Network"),
        sidebarLayout(
          sidebarPanel(
            selectizeInput(
              ns("artist"), 
              "Choose Artist:", 
              choices = NULL,  
              selected = NULL
            ),
            
            selectInput(
              ns("rel_types"), 
              "Relationship Types:",
              choices = NULL,
              selected = c("InStyleOf", "LyricalReferenceTo"), 
              multiple = TRUE
            ),
            sliderInput(ns("depth"), "Network Depth:", min = 1, max = 10, value = 1)
          ),
          mainPanel(
            visNetwork::visNetworkOutput(ns("networkGraph"), height = "350px"),
            br(),
            DT::DTOutput(ns("propagationTable"))
          )
        )
    )
  )
}


mod_network_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # === Graph Preparation ===
    parse_date <- function(x) {
      x <- as.character(x)
      x[x == ""] <- NA
      year_only <- !is.na(x) & grepl("^\\d{4}$", x)
      x[year_only] <- paste0(x[year_only], "-01-01")
      lubridate::ymd(x)
    }
    
    g_nodes_df <- nodes_tbl %>%
      dplyr::rename(
        id = id,
        label = name,
        node_type = node_type  
      ) %>%
      dplyr::mutate(
        id = as.character(id),
        release_date = parse_date(release_date),
        written_date = if ("written_date" %in% names(.)) parse_date(written_date) else as.Date(NA)
      )
    
    g_edges_df <- edges_tbl %>%
      dplyr::rename(from_id = source, to_id = target, relation = edge_type) %>%
      dplyr::mutate(from_id = as.character(from_id), to_id = as.character(to_id))
    
    g_all <- igraph::graph_from_data_frame(
      d = g_edges_df %>% dplyr::transmute(from = from_id, to = to_id, relation),
      vertices = g_nodes_df %>% dplyr::transmute(name = id, label, node_type),
      directed = TRUE
    )
    
    get_collab_influence_graph <- function(artist = "Sailor Shift", rel_types = NULL, depth = 3) {
      if (is.null(rel_types)) rel_types <- igraph::E(g_all)$relation %>% unique()
      g_filt <- igraph::subgraph.edges(g_all, igraph::E(g_all)[igraph::E(g_all)$relation %in% rel_types], delete.vertices = FALSE)
      vid <- which(igraph::V(g_filt)$label == artist)
      if (length(vid) == 0) stop(sprintf("Artist '%s' not found in the graph.", artist))
      vs <- igraph::ego(graph = g_filt, order = depth, nodes = vid, mode = "all")[[1]]
      igraph::induced_subgraph(g_filt, vids = vs)
    }
    
    get_propagation_table <- function(sub_g, artist = "Sailor Shift") {
      vid <- which(igraph::V(sub_g)$label == artist)
      paths <- igraph::shortest_paths(sub_g, from = vid, to = igraph::V(sub_g), mode = "all")$vpath
      tibble::tibble(
        target = sapply(paths, function(p) igraph::V(sub_g)$label[tail(p, 1)]),
        path_length = sapply(paths, length) - 1,
        path = sapply(paths, function(p) paste(igraph::V(sub_g)$label[p], collapse = " → "))
      )
    }
    
    observe({
      req(nodes_tbl, edges_tbl)
      
      # Update Artist Choices
      artist_names <- sort(unique(nodes_tbl$name[nodes_tbl$node_type == "Person"]))
      default_artists <- c("Sailor Shift")
      default_selection <- default_artists[default_artists %in% artist_names]
      
      updateSelectizeInput(
        session = session,
        inputId = "artist",
        choices = artist_names,
        selected = default_selection,
        server = TRUE
      )
      
      # Update Relationship Types
      relation_types <- sort(unique(edges_tbl$edge_type))
      default_rels <- c()
      default_rels <- default_rels[default_rels %in% relation_types]
      
      updateSelectInput(
        session = session,
        inputId = "rel_types",
        choices = relation_types,
        selected = default_rels
      )
    })
    
    # Cache
    cached_graph <- reactiveVal(NULL)
    cached_artist <- reactiveVal(NULL)
    cached_depth <- reactiveVal(NULL)
    cached_types <- reactiveVal(NULL)
    
    subgraph <- reactive({
      req(input$artist)
      if (!is.null(cached_graph()) &&
          cached_artist() == input$artist &&
          identical(cached_depth(), input$depth) &&
          identical(cached_types(), input$rel_types)) {
        return(cached_graph())
      }
      
      sg <- get_collab_influence_graph(input$artist, input$rel_types, input$depth)
      if (igraph::vcount(sg) > 500) {
        showNotification("Subgraph too large, trimming to 500 nodes.", type = "warning")
        sg <- igraph::induced_subgraph(sg, igraph::V(sg)[1:500])
      }
      
      cached_graph(sg)
      cached_artist(input$artist)
      cached_depth(input$depth)
      cached_types(input$rel_types)
      sg
    })
    
    vis_data <- reactive({
      sg <- subgraph()
      verts <- igraph::as_data_frame(sg, what = "vertices") %>%
        dplyr::rename(id = name, title = label) %>%
        dplyr::mutate(group = node_type)
      
      eds <- igraph::as_data_frame(sg, what = "edges") %>%
        dplyr::rename(from = from, to = to, title = relation)
      
      list(nodes = verts, edges = eds)
    })
    
    output$networkGraph <- visNetwork::renderVisNetwork({
      d <- vis_data()
      sg <- subgraph()
      deg <- igraph::degree(sg)
      shapes <- ifelse(deg <= 2, "dot", ifelse(deg <= 5, "triangle", "star"))
      
      nodes_df <- d$nodes %>%
        dplyr::mutate(
          value = deg[id],
          shape = shapes,
          label = title,
          title = paste0("<b>", title, "</b><br>Type: ", group, "<br>Degree: ", deg[id])
        )
      
      visNetwork::visNetwork(nodes_df, d$edges, width = "100%", height = "600px") %>%
        visNetwork::visNodes(
          shapeProperties = list(useBorderWithImage = TRUE),
          font = list(
            size = 18, color = "#2f2f2f", face = "Playfair Display", strokeWidth = 2,
            strokeColor = "#FFFFFF", vadjust = -10
          ),
          scaling = list(min = 10, max = 30),
          borderWidth = 2,
          color = list(
            border = "#2f2f2f",
            background = ifelse(nodes_df$label == input$artist, "#FFD700", "#88CCF1"),
            highlight = "#F6C90E"
          ),
          shadow = TRUE
        ) %>%
        visNetwork::visEdges(
          arrows = list(to = list(enabled = TRUE, scaleFactor = 0.7)),
          smooth = list(enabled = TRUE, type = "dynamic"),
          width = 1.2,
          color = list(color = "#A0A0A0", highlight = "#F6C90E", hover = "#FFA500"),
          shadow = TRUE
        ) %>%
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = FALSE
        ) %>%
        visNetwork::visLegend(
          main = "Node Types",
          position = "right",
          useGroups = TRUE,
          width = 0.2
        ) %>%
        visNetwork::visLayout(randomSeed = 42) %>%
        visNetwork::visInteraction(
          navigationButtons = TRUE,
          hover = TRUE,
          dragNodes = TRUE,
          dragView = TRUE,
          zoomView = TRUE
        ) %>%
        visNetwork::visPhysics(stabilization = TRUE) %>%
        visNetwork::visConfigure(enabled = FALSE)
    })
    
    observeEvent(input$artist, {
      output$propagationTable <- DT::renderDT({
        df <- get_propagation_table(subgraph(), input$artist)
        DT::datatable(df, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE))
      })
    })
  })
}
