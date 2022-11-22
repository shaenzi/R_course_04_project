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
    janitor::clean_names() %>%
    # some NA values are indicated by "-" and therefore the columns are read as char
    dplyr::mutate(dplyr::across(.cols = tidyselect::where(is.character),
                                .fns = as.numeric)) %>%
    #convert year to integer - TBD/todo whether this is correct, first of January is odd, could also be the delivery date
    dplyr::mutate(jahr = lubridate::year(jahr))
  ambulances <- get_csv_from_link("https://data.stadt-zuerich.ch/dataset/sid_srz_hilfsfirsten_rd/download/hilfsfrist_rd.csv") %>%
    janitor::clean_names() %>%
    dplyr::mutate(hilfsfrist_mittelwert = lubridate::hms(hilfsfrist_mittelwert))
  fire_service <- get_csv_from_link("https://data.stadt-zuerich.ch/dataset/sid_srz_ausrueckzeiten_fw/download/ausrueckzeit_fw.csv") %>%
    janitor::clean_names() %>%
    dplyr::mutate(hilfsfrist_mittelwert = lubridate::hms(ausrueckzeit_mittelwert))

  return(list("emergency_calls" = emergency_calls,
              "ambulance" = ambulances,
              "fire_service" = fire_service))
}

#' plot_emergency_calls
#'
#' @param emergency_calls tibble on the emergency calls
#' @param group_selector filters the dat to plot, can be fw, anz, san (?), rd (?)
#'
#' @return ggplot object
#' @noRd
plot_emergency_calls <- function(emergency_calls, group_selector) {
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
#' @param year filter for which year should be selected ('jahr' column in response_times)
#'
#' @return ggplot object
#' @noRd
plot_response_times <- function(response_times, year) {
  response_times <- response_times %>%
    dplyr::filter(jahr == year)

  # prepare stuff for plotting: labels, quantiles etc.
  quantile_vec <- response_times %>%
    dplyr::pull(prozent_einsaetze_bis_10min) %>%
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE)

  labels <- dplyr::tibble(
    lab1 = quantile_vec,
    lab2 = c(quantile_vec[2:length(quantile_vec)], NA)) %>%
    dplyr::slice(1:dplyr::n() - 1) %>% # We remove the last row, since it has no meaning
    dplyr::mutate_all(round, digits = 0) %>% # We remove digits after the 0
    dplyr::mutate(labs = paste(lab1, lab2, sep = " - ")) %>%
    dplyr::mutate(labs = paste0(labs, "%")) %>%
    dplyr::pull(labs)

  # join the two tibbles and plot as a map
  # always need to join with sf as main tibble, otherwise cannot plot
  zurich_kreise %>%
    dplyr::left_join(response_times, by = c("bezeichnun" = "stadtkreis")) %>%
    dplyr::mutate(quantiles = cut(prozent_einsaetze_bis_10min,
                                  breaks = quantile_vec,
                                  labels = labels,
                                  include.lowest = TRUE)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = quantiles),
                     color = "white") +
    ggplot2::scale_fill_viridis_d(option = "E") +
    ggplot2::theme_void()
}


#' bar_chart
#'
#' @description Render a bar chart in a reactable with a label on the left
#'
#' @param label the value
#' @param width the width of the bar to be charted, optional, defaults 100%
#' @param height optional, default 1rem
#' @param fill hex color value, default #00bfc4
#' @param background hex color value, optional, default NULL
#'
#' @return html div with label (number) and bar for bar chart
#' @noRd
bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  bar <- htmltools::div(style = list(background = fill, width = width, height = height))
  chart <- htmltools::div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
}

#' put_data_in_table
#'
#' @description make reactable of ambulance/fire service response times
#'
#' @param response_times ambulance or fire_service tibble
#' @param year filter for which year should be selected ('jahr' column in response_times)
#'
#' @return reactable
#' @noRd
put_data_in_table <- function(response_times, year) {
  response_times <- response_times %>%
    dplyr::filter(jahr == year)
  # could outsource this to a function? though only 2 lines

  max_hilfsfrist <- max(
    lubridate::period_to_seconds(response_times$hilfsfrist_mittelwert)
    )

  response_times %>%
    dplyr::filter(jahr == year) %>%
    dplyr::select(stadtkreis, prozent_einsaetze_bis_10min, hilfsfrist_mittelwert) %>%
    reactable::reactable(columns = list(
      stadtkreis = reactable::colDef(name = "Stadtkreis"),
      prozent_einsaetze_bis_10min = reactable::colDef(
        name = "Anteil Einsätze unter 10min",
        cell = function(value) {
          width <- paste0(value*100 / max(ambulances$prozent_einsaetze_bis_10min), "%")
          value <- paste0(format(round(value, 1), nsmall = 1), "%")
          bar_chart(value, width = width)
        }),
      hilfsfrist_mittelwert = reactable::colDef(
        name = "durchschnittliche Hilfsfrist",
        cell = function(value) {
          width <- paste0(lubridate::period_to_seconds(value) *100 / max_hilfsfrist, "%")
          value <- paste0(lubridate::minute(value),
                          ":",
                          lubridate::second(value))
          bar_chart(value, width = width)
        }
      )
    ),
    pagination = FALSE,
    highlight = TRUE)

  # issue. sorting of char stadtkreise gives 1, 10, 11, 12, 2
  # issue: should really format into %M:%S rather than extracting minutes and seconds,
  #   as it does not align for the fire service --> question

  # Todo: better font, better colors, better column widths

}
