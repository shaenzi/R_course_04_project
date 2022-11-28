#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  mod_response_times_server(id = "ambulance",
                            response_times = data_vector[["ambulance"]],
                            plot_title = "Percentage of patients reached within 10 minutes")
  mod_response_times_server(id = "fire_service",
                            response_times = data_vector[["fire_service"]],
                            plot_title = "Percentage of fire sites reached within 10 minutes")
}
