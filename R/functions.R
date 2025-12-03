
#' Function to create table with descriptive statistics
#'
#' @param data The dataset to create descriptive stastictics on
#'
#' @returns A data.frame/tibble
#
create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) round(x, digits = 1))) |>
    dplyr::mutate(MeanSD = glue::glue("{value_mean} ({value_sd})")) |>
    dplyr::select(Metabolite = metabolite, "Mean SD" = MeanSD)
}





#' Function for plotting distributions of the lipidomics data
#'
#' @param data The lipidomics data to be plotted
#'
#' @returns A plot object
#'
create_plot_distributions <- function(data) {
  data |> ggplot2::ggplot(
    ggplot2::aes(x = value)
  ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free") +
    ggplot2::theme_minimal()
}
