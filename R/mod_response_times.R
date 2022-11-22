#' response_times UI Function
#'
#' @description A shiny Module for showing response times on a map and in a reactable
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_response_times_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(6,
                  plotOutput("map")),
           column(6,
                  reactable::reactableOutput("table"))
           # Todo add input for year selection

  )
}

#' response_times Server Functions
#'
#' @param response_times tibble with response times, ambulances or fire_service
#'
#' @noRd
mod_response_times_server <- function(id, response_times){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$map <- renderPlot({
      plot_response_times(response_times, 2020)
    })

    output$table <- renderReactable({
      put_data_in_table(response_times, 2020)
    })
    # TODO: test!

  })
}

## To be copied in the UI
# mod_response_times_ui("response_times_1")

## To be copied in the server
# mod_response_times_server("response_times_1")
