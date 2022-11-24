
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

