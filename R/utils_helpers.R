#' get_csv_from_link
#'
#' @description function to read a csv function from a file/link
#'
#' @param my_url Parameter specifying the csv file's url.
#'
#' @return returns the data from the csv as a tibble
#'
#' @noRd
get_csv_from_link <- function(my_url){
  data <- readr::read_csv(my_url, show_col_types = FALSE) %>%
    janitor::clean_names()
}

#' get_data
#'
#' @description Function to read the three open government datasets on which the app is based
#'
#' @details The sources of the datasets are the 'Einsatzstatistik' (https://data.stadt-zuerich.ch/dataset/sid_srz_einsatzstatistik/resource/79dad524-6aa2-4a70-921f-a39a3b83b438),
#' the data on how long it takes the fire service to reach the different parts of the city (https://data.stadt-zuerich.ch/dataset/sid_srz_ausrueckzeiten_fw/resource/c2c13476-f8db-4621-8b57-79807836fb88),
#' and how long it takes the ambulances (https://data.stadt-zuerich.ch/dataset/sid_srz_hilfsfirsten_rd/resource/f3ec47af-af8e-4002-b9b1-7b7d85d5f7f2)
#'
#' @return a named list of tibbles with emergency_calls, ambulance, and fire_service
#' @noRd
get_data <- function() {
  emergency_calls <- get_csv_from_link("https://data.stadt-zuerich.ch/dataset/sid_srz_einsatzstatistik/download/SRZ_einsatzstatistik_seit2010.csv") %>%
    # some NA values are indicated by "-" and therefore the columns are read as char
    dplyr::mutate(dplyr::across(.cols = tidyselect::where(is.character),
                                .fns = as.numeric)) %>%
    #convert year to integer - TBD/todo whether this is correct, first of January is odd, could also be the delivery date
    dplyr::mutate(jahr = lubridate::year(jahr))
  ambulances <- get_csv_from_link("https://data.stadt-zuerich.ch/dataset/sid_srz_hilfsfirsten_rd/download/hilfsfrist_rd.csv")
  fire_service <- get_csv_from_link("https://data.stadt-zuerich.ch/dataset/sid_srz_ausrueckzeiten_fw/download/ausrueckzeit_fw.csv")

  return(list("emergency_calls" = emergency_calls,
              "ambulance" = ambulances,
              "fire_service" = fire_service))
}

#' plot_calls
#'
#' @param emergency_calls tibble on the emergency calls
#' @param group_selector filters the dat to plot, can be fw, anz, san (?), rd (?)
#'
#' @return ggplot object
#' @noRd
plot_calls <- function(emergency_calls, group_selector) {
  emergency_calls %>%
    dplyr::select(jahr, dplyr::starts_with(group_selector)) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(group_selector)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = jahr, y = value, color = name)) +
    ggplot2::geom_line()
  #todo improve data selection and plotting. area plot? not sure what to do with the stuff that only is present for some years
}

#' plot_response_times
#'
#' @param response_times ambulance or fire_service tibble
#'
#' @return ggplot object
#' @noRd
plot_response_times <- function(response_times) {
  #todo
}
