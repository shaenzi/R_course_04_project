library(leaflet)

data_vector <- get_zurich_data()

response_times <- data_vector[["ambulance"]]

response_times <- response_times %>%
  dplyr::filter(jahr == 2020) %>%
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

zurich_kreise %>%
  sf::st_as_sf() %>%
  sf::st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
  dplyr::left_join(response_times %>% dplyr::filter(jahr == 2020),
                   by = c("bezeichnun" = "stadtkreis_string")) %>%
  dplyr::arrange(stadtkreis) %>%
  leaflet::leaflet() %>%
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
