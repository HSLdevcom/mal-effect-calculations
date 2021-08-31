# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

region <- readr::read_rds(here::here("results", "region.rds"))
bonus_region <- readr::read_rds(here::here("results", "bonus_region.rds"))
metro <- readr::read_rds(here::here("results", "metro.rds"))
train <- readr::read_rds(here::here("results", "train.rds"))
roads <- readr::read_rds(here::here("results", "roads.rds"))
municipalities <- readr::read_rds(here::here("results", "municipalities.rds"))
water <- readr::read_rds(here::here("results", "water.rds"))

bbox <- sf::st_bbox(region)

geom_basemap <- function() {
  list(
    geom_sf(data = water, fill = "#a6cee3", color = NA, size = 0),
    geom_sf(data = municipalities, color = "#9b3b6b", fill = NA, size = 0.26),
    geom_sf(data = roads, color = "#64a5a5", size = 0.2),
    geom_sf(data = train, color = "#000000", linetype = "solid", size = 0.66),
    geom_sf(data = train, color = "#ffffff", linetype = "22", size = 0.53),
    geom_sf(data = metro, color = "#ff7f00", linetype = "solid", size = 0.66),
    geom_sf(data = metro, color = "#ffffff", linetype = "22", size = 0.53),
    geom_sf(data = region, color = "#000000", fill = NA, size = 0.46),
    geom_sf(data = bonus_region, color = "#000000", fill = NA, linetype = "33", size = 0.46),
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
  caption <- paste(c("Maastotietokanta: Maanmittauslaitos 2021"),
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

theme_mal_map <- function() {
  list(
    theme_void(),
    theme(
      text = element_text(family = "sans", colour = "#333333", size = 10),
      plot.title = element_text(colour = "#3E8606"),
      legend.position = c(0.00, 0.90),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.margin = margin(3, 3, 3, 3),
      legend.text = element_text(size = rel(1.0)),
      plot.caption = element_text(size = rel(1.0))
    )
  )
}

theme_mal_graph <- function() {
  list(
    theme_minimal(),
    theme(
      text = element_text(family = "sans", colour = "#333333", size = 10),
      plot.title = element_text(colour = "#3E8606"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = rel(1.0)),
      plot.caption = element_text(size = rel(1.0)),
      strip.placement = "outside",
      panel.grid.major.x = element_blank(),
      #strip.switch.pad.grid = unit(0, "cm"),
      panel.spacing = unit(0, "lines")
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
                         width = 148,
                         height = 105,
                         units = "mm",
                         dpi = 600, ...) {
  ggsave(filename = filename,
         width = width,
         height = height,
         units = units,
         dpi = dpi,
         ...)
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
