#' plot_response_times
#'
#' @param response_times ambulance or fire_service tibble
#' @param year filter for which year should be selected ('jahr' column in response_times)
#'
#' @return ggplot object
#' @noRd
plot_response_times <- function(response_times, year, title) {
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
    sf::st_as_sf() %>%
    dplyr::left_join(response_times, by = c("bezeichnun" = "stadtkreis")) %>%
    dplyr::mutate(quantiles = cut(prozent_einsaetze_bis_10min,
                                  breaks = quantile_vec,
                                  labels = labels,
                                  include.lowest = TRUE)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = quantiles),
                     color = "white") +
    #ggplot2::scale_fill_viridis_d(option = "E", name = NULL) +
    ggplot2::scale_fill_brewer(palette = "OrRd", direction = -1) +
    #ggplot2::scale_fill_brewer(palette = "RdBu") +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="bottom")
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

  max_hilfsfrist <- max(response_times$hilfsfrist_sec)

  response_times %>%
    dplyr::filter(jahr == year) %>%
    dplyr::select(stadtkreis, prozent_einsaetze_bis_10min, hilfsfrist_sec) %>%
    reactable::reactable(columns = list(
      stadtkreis = reactable::colDef(
        name = "urban district",
        minWidth = 60,
        filterMethod = htmlwidgets::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return Number(row.values[columnId].slice(-2))
        })
      }")),
      prozent_einsaetze_bis_10min = reactable::colDef(
        name = "proportion of response times below 10min",
        cell = function(value) {
          width <- paste0(value*100 / max(response_times$prozent_einsaetze_bis_10min), "%")
          value <- paste0(format(round(value, 1), nsmall = 1), "%")
          bar_chart(value, width = width)
        }),
      hilfsfrist_sec = reactable::colDef(
        name = "mean response time",
        cell = function(value) {
          width <- paste0(value *100 / max_hilfsfrist, "%")
          # hacky way to specify the hours and minutes to make sure there are always two digits each
          value <- format(Sys.Date() + lubridate::seconds_to_period(value), "%M:%S")
          bar_chart(value, width = width)
        }
      )
    ),
    pagination = FALSE,
    highlight = TRUE)

  # issue. sorting of char stadtkreise gives 1, 10, 11, 12, 2

  # Todo: better font, better colors, better column widths
}
