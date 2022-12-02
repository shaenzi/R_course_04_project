library(plotly)
# https://plotly.com/r/choropleth-maps/#customize-choropleth-chart
response_times <- ambulance

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

response_times <- response_times %>%
  dplyr::mutate(quantiles = cut(prozent_einsaetze_bis_10min,
                                breaks = quantile_vec,
                                labels = labels,
                                include.lowest = TRUE))

colors <- c("#fef0d9",
            "#fdcc8a",
            "#fc8d59",
            "#e34a33",
            "#b30000")

names(colors) <- labels


g <- list(
  fitbounds = "locations",
  visible = FALSE,
  projection = list(type = 'winkel tripel')
)

# available map projections: https://plotly.com/python/map-configuration/#map-projections


fig <- plot_ly()

fig <- fig %>% add_trace(
  type="choropleth",
  geojson=zurich_kreise,
  locations=response_times$stadtkreis_string,
  z=response_times$quantiles, # quantiles do not work??
  colorscale=colors,#"Viridis",
  featureidkey="properties.bezeichnung",
  zmin=min(response_times$prozent_einsaetze_bis_10min),
  zmax=100,
  marker=list(line=list(
    width=0)
  )
)

fig <- fig %>% layout(
  geo = g,
  title = "bla"
)

fig

# https://stackoverflow.com/questions/63635104/plotly-how-to-set-choropleth-map-color-for-a-discrete-categorical-variable

# https://community.plotly.com/t/discrete-colors-for-choroplethmapbox-with-plotly-graph-objects/37989

# or do the opposite and try to animate the ttplot figure?

# could also plot sf... https://plotly.com/ggplot2/maps-sf/
