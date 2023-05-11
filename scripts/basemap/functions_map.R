# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

# 2018, 2040 ve0, ve1, ve2
mal_greens_fill <- c("#CEE0C0", "#9EC282", "#6EA444", "#3E8606")
mal_color <- c("#333333", "#333333", "#333333", "#FFFFFF")
hsl_blues_fill <- c("#BFDDF1", "#7FBCE3", "#3F9BD6", "#007AC9")
hsl_greens_fill <- c("#D8EEC6", "#B1DE8E", "#8ACE56", "#64BE1E")
hsl_teals_fill <- c("#BFEDF8", "#7FDCF1", "#3FCAEA", "#00B9E4")
hsl_yellows_fill <- c("#FEEDC5", "#FDDC8C", "#FCCA52", "#FCB919")

region <- readr::read_rds(here::here("results", "region.rds"))
bonus_region <- readr::read_rds(here::here("results", "bonus_region.rds"))
metro <- readr::read_rds(here::here("results", "metro.rds"))
metro_2040_ve0 <- readr::read_rds(here::here("results", "metro_2040_ve0.rds"))
train <- readr::read_rds(here::here("results", "train.rds"))
roads <- readr::read_rds(here::here("results", "roads.rds"))
municipalities <- readr::read_rds(here::here("results", "municipalities.rds"))
water <- readr::read_rds(here::here("results", "water.rds"))

bbox <- sf::st_bbox(region)

geom_basemap <- function(show_roads = TRUE) {
  list(
    geom_sf(data = water, fill = "#a6cee3", color = NA, size = 0),
    geom_sf(data = municipalities, color = "#9b3b6b", fill = NA, size = 0.26),
    if (show_roads) { geom_sf(data = roads, color = "#64a5a5", size = 0.2) },
    geom_sf(data = train, color = "#000000", linetype = "solid", size = 0.66),
    geom_sf(data = train, color = "#ffffff", linetype = "22", size = 0.53),
    geom_sf(data = metro, color = "#ff7f00", linetype = "solid", size = 0.66),
    geom_sf(data = metro, color = "#ffffff", linetype = "22", size = 0.53),
    geom_sf(data = metro_2040_ve0, color = "#ff7f00", linetype = "solid", size = 0.66),
    geom_sf(data = metro_2040_ve0, color = "#ffffff", linetype = "22", size = 0.53),
    geom_sf(data = region, color = "#000000", fill = NA, size = 0.46),
    geom_sf(data = bonus_region, color = "#000000", fill = NA, linetype = "33", size = 0.46)
  )
}

coord_sf_mal <- function() {
  list(
    coord_sf(
      xlim = c(bbox$xmin - 500, bbox$xmax + 500),
      ylim = c(bbox$ymin + 20000, bbox$ymax + 500),
      expand = FALSE,
      datum = sf::st_crs(3879)
    )
  )
}

points2mm <- function(x) {
  return(x / (72.27 / 25.4))
}

annotate_map <- function(title, subtitle) {
  # Aineiston nimi: Lisenssin antaja 20XX
  caption <- paste(c("Maastotietokanta: Maanmittauslaitos 2021",
                     "Maastokartta 1:100 000: Maanmittauslaitos 2021"),
                   collapse = "\n")
  list(
    # Title
    annotate(
      "text",
      x = bbox$xmin,
      y = bbox$ymax,
      hjust = 0,
      vjust = 1,
      label = title,
      size = points2mm(10),
      colour = "#3E8606"
    ),
    # Subtitle
    annotate(
      "text",
      x = bbox$xmin,
      y = bbox$ymax - 4000,
      hjust = 0,
      vjust = 1,
      label = subtitle,
      size = points2mm(10),
      colour = "#333333"
    ),
    # Caption
    annotate(
      "label",
      x = bbox$xmax,
      y = bbox$ymin + 20000,
      hjust = 1,
      vjust = 0,
      label = caption,
      size = points2mm(8),
      color = "#333333",
      label.size = 0
    )
  )
}

scale_volume <- function(binwidth, height, labels, x0, y0) {

  #             ____
  #        ____|    | h
  #   ____|    |    | h
  #  |____|____|____| h
  #    w    w    w

  n <- length(labels) - 1

  x <- rep(c(0, 1, 1, 0), times = n)
  shift <- rep(1:n, each = 4) - 1
  x <- (x + shift) * binwidth + x0

  y <- rep(c(0, 0, 1, 1), times = n)
  shift <- rep(1:n, each = 4)
  y <- (y * shift) * height + y0

  volume_bars <- data.frame(x = x, y = y, group = rep(1:n, each = 4))

  x <- (1:(n+1) - 0.5) * binwidth + x0
  y <- y0 - 1000

  volume_labels <- data.frame(x = x, y = y, label = labels)

  return(list(
    geom_polygon(mapping = aes(x = x, y = y, group = group),
                 data = volume_bars, fill = "#2a5475"),
    geom_text(mapping = aes(x = x, y = y, label = label),
              data = volume_labels, vjust = 1, hjust = 0.5,
              size = points2mm(10),
              colour = "#333333")
  ))
}

theme_mal_map <- function() {
  list(
    theme_void(),
    theme(
      plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      text = element_text(family = "sans", colour = "#333333", size = 10),
      plot.title = element_text(colour = "#3E8606"),
      legend.position = c(0.00, 0.90),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.margin = margin(3, 3, 3, 3),
      legend.text = element_text(size = rel(1.0)),
      plot.caption = element_text(size = rel(1.0)),
      legend.key.height= unit(1, "cm")
    )
  )
}

theme_mal_graph <- function() {
  list(
    theme_minimal(),
    theme(
      plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      text = element_text(family = "sans", colour = "#333333", size = 10),
      plot.title = element_text(colour = "#3E8606", size = 10),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = rel(1.0)),
      plot.caption = element_text(size = rel(1.0)),
      strip.placement = "outside",
      panel.grid.major.x = element_blank(),
      #strip.switch.pad.grid = unit(0, "cm"),
      panel.spacing = unit(0, "lines"),
      axis.text = element_text(colour = "#333333"),
      strip.text = element_text(colour = "#333333"),
      panel.grid = element_line(colour = "#dddddc")
    )
  )
}

ggsave_map <- function(filename,
                       width = 148,
                       height = 169,
                       units = "mm",
                       dpi = 600, ...) {
  ggsave(filename = filename,
         width = width,
         height = height,
         units = units,
         dpi = dpi,
         ...)
}

ggsave_graph <- function(filename,
                         width = 155,
                         height = 71,
                         units = "mm",
                         dpi = 600, ...) {
  ggsave(filename = filename,
         width = width,
         height = height,
         units = units,
         dpi = dpi,
         ...)
}
