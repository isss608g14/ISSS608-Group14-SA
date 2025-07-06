#' Launch
#' 
#' Launch Shiny Application
#'
#' @export
#' @import fullPage
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_oceanus <- function() {
  data("nodes_tbl", package = "oceanus.app")
  data("edges_tbl", package = "oceanus.app")
  
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list()
  )
}

