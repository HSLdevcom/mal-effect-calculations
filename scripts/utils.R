# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


read_tsv_helmet <- function(..., first_col_name, comment = "#") {
  withCallingHandlers({
    readr::read_tsv(..., comment = comment) %>%
      dplyr::rename({{first_col_name}} := X1) %>%
      dplyr::rename_with(~ gsub("-", "_", .x, fixed = TRUE))
  }, warning = function(w) {
    # Helmet results never include first column name
    if (conditionMessage(w) == "Missing column names filled in: 'X1' [1]") {
      invokeRestart("muffleWarning")
    }
  })
}

read_helmet_omx <- function(path) {
  zone_numbers <- omxr::read_lookup(path, name = "zone_number")
  zone_numbers <- as.vector(zone_numbers$Lookup, mode = "integer")
  omx_matrix <- omxr::read_all_omx(path) %>%
    dplyr::mutate(
      origin = zone_numbers[origin],
      destination = zone_numbers[destination]
    )
  return(omx_matrix)
}

verbose_source <- function(file, ...) {
  message(sprintf("Running analysis in %s...", basename(file)))
  invisible(source(file, ...))
}

scale_to_range <- function(x, xmin = min(x), xmax = max(x), a, b) {
  # Scales vector x linearly to range [a, b] so that xmin = a and xmax = b.
  # Usually, xmin and xmax are min(x) and max(x) but they can be other values
  # too.
  return((b - a) * (x - xmin) / (xmax - xmin) + a)
}


# Functions to calculate accessibility to two centers ---------------------

scale_and_squish <- function(x, xrange, a, b) {
  x_normal <- scale_to_range(x, xrange[1], xrange[2], a, b)
  x_normal_ab <- pmax(a, pmin(x_normal, b))
  return(x_normal_ab)
}

break_twocenters <- function(x) {
  unique(quantile(x, probs = seq.int(0, 1, by = 1 / 5)))
}

cut_twocenters <- function(x, breaks) {
  cut(x, breaks = breaks, include.lowest = TRUE)
}

ttime_to_bins <- function(x, xrange, a, b, breaks) {
  x_normal_ab <- scale_and_squish(x, xrange, a, b)
  return(cut_twocenters(x_normal_ab, breaks = breaks))
}

plot_twocenters <- function(data, fill, title) {
  ggplot() +
    geom_sf(mapping = aes(fill = {{ fill }}),
            data = data, color = NA) +
    scale_fill_viridis_d(
      option="plasma",
      name = "indeksi",
      direction = -1
    ) +
    geom_basemap() +
    coord_sf_mal() +
    annotate_map(
      title = title,
      subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
    ) +
    theme_mal_map()
}

twocenters <- function(.data, mode, title) {
  column <- sprintf("ttime_twocenters_%s", mode)
  # If we are on baseline scenario, calculate range of the variable. Otherwise
  # read it from the result folder.
  .data_to_range <- .data %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(!(zone %in% outlier_zones))
  if (config::get("scenario") == config::get("baseline_scenario")) {
    xrange <- range(.data_to_range[[column]])
    readr::write_rds(xrange, file = here::here("results", sprintf("twocenters_range_%s.rds", mode)))
  } else {
    message("twocenters: read ranges...")
    xrange <- readr::read_rds(here::here("results", sprintf("twocenters_range_%s.rds", mode)))
  }
  # Normalise variable.
  ttime_twocenters_normal = scale_and_squish(.data[[column]],
                                             xrange = xrange,
                                             a = a,
                                             b = b)
  # If we are on baseline scenario, calculate breaks of the variable for the
  # plotting. Otherwise read it from the result folder.
  if (config::get("scenario") == config::get("baseline_scenario")) {
    breaks <- break_twocenters(ttime_twocenters_normal)
    readr::write_rds(breaks, file = here::here("results", sprintf("twocenters_breaks_%s.rds", mode)))
  } else {
    message("twocenters: read breaks...")
    breaks <- readr::read_rds(here::here("results", sprintf("twocenters_breaks_%s.rds", mode)))
  }
  # Cut variable into bins and plot.
  .data$bins = cut_twocenters(ttime_twocenters_normal,
                              breaks = breaks)
  plot_twocenters(.data, bins, title)
  ggsave_map(here::here("figures", sprintf("map_twocenters_%s_%s.png", mode, config::get("scenario"))))
  return(ttime_twocenters_normal)
}
