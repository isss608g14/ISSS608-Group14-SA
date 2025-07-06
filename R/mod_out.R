#' out Module
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

mod_out_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Add UI components here
  )
}


mod_out_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Add server logic here
  })
}