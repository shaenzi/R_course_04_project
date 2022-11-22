#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  data_vector <- get_zurich_data()
  # This does not work. probably not run yet?

  mod_response_times_server("ambulance", data_vector[["ambulance"]])
  mod_response_times_server("fire_service", data_vector[["fire_service"]])
}
