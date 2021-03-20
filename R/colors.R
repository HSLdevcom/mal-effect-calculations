# -*- coding: utf-8-unix -*-
library(tidyverse)

# Colors from:
# https://www.hsl.fi/sites/default/files/tyyliopas/hsl-varit-2015.pdf
# HSL: September 2018

hsl_colors <- c(
  # brand
  blue = "#007AC9",
  lightblue = "#bee4f8",
  pink = "#f092cd",
  lightpink = "#f4deec",
  green = "#64be1e",
  lightgreen = "#d0e6be",
  gray = "#999999",
  lightgray = "#dddddd",
  red = "#dc0451",
  white = "#FFFFFF",
  # transit modes
  tram = "#00985f",
  ferry = "#00b9e4",
  metro = "#ff6319",
  train = "#8c4799",
  lrt = "#00b2a9"
)

scales::show_col(hsl_colors)

hsl_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols)) {
    return(unname(hsl_colors))
  }
  unname(hsl_colors[cols])
}

hsl_palettes <- list(
  red_to_blue = hsl_cols("red", "white", "blue"),
  grey_red_blue = hsl_cols("gray", "red", "blue"),
  reds = hsl_cols("lightpink","pink"),
  blues = hsl_cols("lightblue", "blue")
)

hsl_pal <- function(palette = "blues", reverse = FALSE, ...) {
  pal <- hsl_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

hsl_blues_percent <- function(p) {
  stopifnot(p >= 0 & p <= 1)
  p <- round(p, digits = 2)
  i <- as.integer(100 * p)
  return(hsl_pal("blues")(101)[i + 1])
}
