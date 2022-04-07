# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


set_scenario <- function(scenario) {
  stopifnot(length(scenario) == 1)
  dplyr::filter(scenarios, scenario == !!scenario)
}

read_and_bind <- function(scenario_list, prefix, suffix = "rds") {
  # Read files
  file_names <- sprintf("%s_%s.%s", prefix, scenario_list, suffix)
  files <- lapply(file_names, function(x) { readr::read_rds(here::here("results", x)) })
  # Get human-readable name
  m <- match(scenario_list, scenarios$scenario)
  scenario_names <- sprintf("%i %s", scenarios$year[m], scenarios$name[m])
  names(files) <- scenario_names
  # Bind all and add human-readable name to `scenario`
  all <- dplyr::bind_rows(files, .id = "scenario") %>%
    dplyr::mutate(scenario = forcats::as_factor(scenario))
}

read_tsv_helmet <- function(..., first_col_name, comment = "#") {
  withCallingHandlers({
    readr::read_tsv(..., comment = comment) %>%
      dplyr::rename({{first_col_name}} := `...1`) %>%
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

scale_to_range <- function(x, xmin = min(x, na.rm = TRUE), xmax = max(x, na.rm = TRUE), a, b) {
  # Scales vector x linearly to range [a, b] so that xmin = a and xmax = b.
  # Usually, xmin and xmax are min(x) and max(x) but they can be other values
  # too.
  return((b - a) * (x - xmin) / (xmax - xmin) + a)
}


scale_accessibility <- function(.data) {
  # Scaling to 0-100
  a <- 0
  b <- 100
  # If we are on baseline scenario, calculate range of the variable. Otherwise
  # read it from the result folder.
  .data <- .data %>%
    sf::st_drop_geometry() %>%
    # Dropping outlier areas with little population
    dplyr::mutate(accessibility = dplyr::if_else(total_pop > 15, accessibility, NA_real_))
  if (scenario_attributes[["present"]]) {
    xrange <- range(.data[["accessibility"]], na.rm = TRUE)
    readr::write_rds(xrange, file = here::here("results", "accessibility_range.rds"))
  } else {
    message("accessibility: read ranges...")
    xrange <- readr::read_rds(here::here("results", "accessibility_range.rds"))
  }
  # Normalise variable.
  accessibility = scale_to_range(.data[["accessibility"]],
                                 xmin = xrange[1],
                                 xmax = xrange[2],
                                 a = a,
                                 b = b)
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

twocenters <- function(.data, mode) {
  # Scaling to 0-100
  a <- 0
  b <- 100
  # Dropping Suomenlinna because it distorts the results too much.
  outlier_zones <- c(1531)

  column <- sprintf("ttime_twocenters_%s", mode)
  # If we are on baseline scenario, calculate range of the variable. Otherwise
  # read it from the result folder.
  .data_to_range <- .data %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(!(zone %in% outlier_zones))
  if (scenario_attributes[["present"]]) {
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
  if (scenario_attributes[["present"]]) {
    breaks <- break_twocenters(ttime_twocenters_normal)
    readr::write_rds(breaks, file = here::here("results", sprintf("twocenters_breaks_%s.rds", mode)))
  } else {
    message("twocenters: read breaks...")
    breaks <- readr::read_rds(here::here("results", sprintf("twocenters_breaks_%s.rds", mode)))
  }
  # Cut variable into bins and plot.
  bins = cut_twocenters(ttime_twocenters_normal,
                        breaks = breaks)
  return(list(ttime_twocenters_normal = ttime_twocenters_normal,
              bins_twocenters = bins))
}
