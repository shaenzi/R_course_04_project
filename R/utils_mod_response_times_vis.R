#' plot_response_times
#'
#' @param response_times ambulance or fire_service tibble
#' @param year filter for which year should be selected ('jahr' column in response_times)
#'
#' @return ggplot object
#' @noRd
plot_response_times <- function(response_times, year, title) {
  response_times <- response_times %>%
    dplyr::filter(jahr == year) %>%
    dplyr::arrange(stadtkreis)

  # prepare stuff for plotting: labels, quantiles etc.
  quantile_vec <- response_times %>%
    dplyr::pull(prozent_einsaetze_bis_10min) %>%
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE)

  pal <- leaflet::colorBin("YlOrRd",
                           domain = response_times$prozent_einsaetze_bis_10min,
                           bins = quantile_vec,
                           reverse = TRUE)

  labels <- sprintf(
    "<strong>%s</strong><br/>%g ",
    response_times$stadtkreis_string, response_times$prozent_einsaetze_bis_10min
  ) %>% lapply(htmltools::HTML)


  # join the two tibbles and plot as a map
  # always need to join with sf as main tibble, otherwise cannot plot
  # need to sort after joining otherwise labels are wrong
  zurich_kreise %>%
    sf::st_as_sf() %>%
    sf::st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
    dplyr::left_join(response_times,
                     by = c("bezeichnun" = "stadtkreis_string")) %>%
    dplyr::arrange(stadtkreis) %>%
    leaflet::leaflet() %>%
    #leaflet::fitBounds(8.45, 47.33, 8.55, 47.43) %>%
    leaflet::setView(8.54, 47.377, 12) %>%
    #leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~pal(prozent_einsaetze_bis_10min),
      weight = 2,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      highlightOptions = leaflet::highlightOptions(
        weight = 5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE),
    ) %>%
    leaflet::addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                       position = "bottomright",
                       labFormat = leaflet::labelFormat(suffix = "%", digits = 0))
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
bar_chart <- function(label, width = "100%", height = "1rem", fill = "#0d0599", background = "#e1e1e1") {
  bar <- htmltools::div(style = list(background = fill, width = width, height = height))
  chart <- htmltools::div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
}

#' kreis_labels
#'
#' @param value
#'
#' @return html div with the "Kreis X" when x is the value
#' @noRd
#'
kreis_labels <- function(value) {
  label <- paste("Kreis", as.character(value))
  htmltools::div(style=list(display = "flex"), label)
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
        cell = function(value){
          kreis_labels(value)
        }),
      prozent_einsaetze_bis_10min = reactable::colDef(
        name = "proportion of response times below 10min",
        cell = function(value) {
          width <- paste0(value*100 / max(response_times$prozent_einsaetze_bis_10min), "%")
          value <- paste0(format(round(value, 1), nsmall = 1), "%")
          bar_chart(value, width = width)
        }),
      hilfsfrist_sec = reactable::colDef(
        name = "mean response time [min:sec]",
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
