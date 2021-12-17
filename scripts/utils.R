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

scale_to_range <- function(x, xmin, xmax, a, b) {
  # Scales vector x linearly to range [a, b] so that xmin = a and xmax = b.
  # Usually, xmin and xmax are min(x) and max(x) but they can be other values
  # too.
  return((b - a) * (x - xmin) / (xmax - xmin) + a)
}
