#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  ambulance_choices <- unique(data_vector[["ambulance"]]$jahr)
  fire_service_choices <- unique(data_vector[["fire_service"]]$jahr)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    navbarPage(
      theme = bslib::bs_theme(version = 4, bootswatch = "sandstone"), # or sandstone
      h1("How quickly does help reach you?"),
      header = p("Response times of the emergency services in the city of Zurich."),
      tabPanel("Ambulance",
               mod_response_times_ui(id = "ambulance",
                                     title = "Ambulance response times",
                                     choices = ambulance_choices)),
      tabPanel("Fire Services",
        mod_response_times_ui(id = "fire_service",
                              title = "Fire service response times",
                              choices = fire_service_choices)),
      tabPanel("About",
               includeMarkdown(system.file("app/www/about.md", package = "c4app")))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "c4app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
