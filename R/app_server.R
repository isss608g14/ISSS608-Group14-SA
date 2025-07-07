#' @import shiny
app_server <- function(input, output, session) {
  echarts4r::e_common(
    font_family = "Playfair Display",
    theme = "vintage"
  )
  
  mod_artist_server("artist")
  mod_network_server("network")
  mod_trend_server("trend")
  callModule(mod_star_server, "star")
  mod_feature_server("feature")
  mod_whatif_server("whatif")
}
