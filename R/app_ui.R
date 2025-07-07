#' @import shiny
app_ui <- function() {
  
  options <- list()
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    pagePiling(
      sections.color = c('#f9f7f1', '#f9f7f1', '#f9f7f1', '#f9f7f1', '#f9f7f1', '#f9f7f1', '#f9f7f1', '#2a5a5b'),
      opts = options,
      menu = c(
        "Home" = "home",
        "How do artists differ?" = "artist",
        "Who connects whom?" = "network",
        "What’s the trend?" = "trend",
        "Who’s rising?" = "star",
        "Why are they rising?" = "feature",
        "What if they...?" = "whatif",
        "About" = "about"
      ),
      pageSection(
        menu = "home",
        center = TRUE,
        tags$div(
          style = "position: relative; height: 100vh; overflow: hidden;",
          
          # Fullscreen background video
          tags$video(
            src = "www/img/cover.mp4",
            type = "video/mp4",
            autoplay = NA,
            muted = NA,
            loop = NA,
            playsinline = NA,
            style = "
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        object-fit: cover;
        z-index: -1;"
          ),
          
          # Main title centered vertically and horizontally
          tags$div(
            style = "
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        text-align: center;
        color: #f9f7f1;
        font-family: 'Playfair Display', serif;
        z-index: 2;",
            h1("The Sound that Sailed", class = "header shadow-dark")
          ),
          
          # Footer text fixed to bottom center
          tags$div(
            style = "
        position: absolute;
        bottom: 30px;
        left: 50%;
        transform: translateX(-50%);
        color: #f9f7f1;
        font-family: 'Playfair Display', serif;
        font-size: 1.1em;
        z-index: 2;",
            "by Group 14, Tai Yu Ying, Liu Hao & Sandra Jacob"
          )
        )
      ),
      pageSection(
        center = TRUE,
        menu = "artist",
        mod_artist_ui("artist")
      ),
      pageSection(
        center = TRUE,
        menu = "network",
        mod_network_ui("network")
      ),
      pageSection(
        center = TRUE,
        menu = "trend",
        mod_trend_ui("trend")
      ),
      pageSection(
        center = TRUE,
        menu = "star",
        mod_star_ui("star")
      ),
      pageSection(
        center = TRUE,
        menu = "feature",
        mod_feature_ui("feature")
      ),
      pageSection(
        center = TRUE,
        menu = "whatif",
        mod_whatif_ui("whatif")
      ),
      pageSection(
        center = TRUE,
        menu = "about",
        h1("About", class = "header shadow-dark"),
        h2(
          class = "shadow-light",
          tags$a("The code", href = "https://github.com/isss608g14/ISSS608-Group14-SA", target = "_blank", class = "link"),
          "|",
          tags$a("The project", href = "https://thesoundthatsailed.netlify.app/", target = "_blank", class = "link")
        ),
        tags$div(
          style = "
        position: absolute;
        bottom: 30px;
        left: 50%;
        transform: translateX(-50%);
        color: #f9f7f1;
        font-family: 'Playfair Display', serif;
        font-size: 1.1em;
        text-align: center;
        margin-top: 2rem;
  ",
          "by Group 14, Tai Yu Ying, Liu Hao & Sandra Jacob"
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'oceanus.app')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(
      rel = "stylesheet", href = shinythemes::shinytheme("sandstone")
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/style.css"),
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=UA-74544116-1"),
    tags$script(
      "window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-74544116-1');"
    )
  )
}
