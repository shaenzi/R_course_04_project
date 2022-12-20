#' response_times UI Function
#'
#' @description A shiny Module for showing response times on a map and in a reactable
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param title H2 title to show at the top of the module ui
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_response_times_ui <- function(id, title, choices, notes=""){
  ns <- NS(id)
  fluidPage(
    tags$head(
      # styling for leaflet background
      tags$style(HTML("
      .leaflet-container {
    background: #FFF;
      }"))
    ),
    p("Response times of the emergency services in the city of Zurich."),
    h2(title),
    fluidRow(column(6,
                    radioButtons(ns("year_switch"),
                                label = "choose a year",
                                choices = choices,
                                selected = choices[1],
                                inline = TRUE),
                    leaflet::leafletOutput(ns("map"))),
             column(6,
                    reactable::reactableOutput(ns("table")))
  ),
  fluidRow(htmltools::p(notes)))
}

#' response_times Server Functions
#'
#' @param response_times tibble with response times, ambulances or fire_service
#' @param plot_title String to place as title to the map
#'
#' @noRd
mod_response_times_server <- function(id, response_times, plot_title){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$map <- leaflet::renderLeaflet({
      plot_response_times(response_times,
                          as.numeric(input$year_switch),
                          plot_title)
    })

    output$table <- reactable::renderReactable({
      put_data_in_table(response_times, as.numeric(input$year_switch))
    })

  })
}

## To be copied in the UI
# mod_response_times_ui("response_times_1")

## To be copied in the server
# mod_response_times_server("response_times_1")

#
# ui <- fluidPage(
#       mod_response_times_ui("ambulance")
# )
#
# server <- function(input, output, session) {
#   data_vector <- get_zurich_data()
#   # This does not work. probably not run yet?
#
#   mod_response_times_server("ambulance", data_vector[["ambulance"]])
# }
