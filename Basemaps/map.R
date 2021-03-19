# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("Basemaps", "functions_map.R"), encoding = "utf-8")

zones <- readr::read_rds(here::here("Basemaps", "zones.rds"))

centroids <- zones %>%
  sf::st_centroid()

p <- sf::st_point(c(25492241, 6681165)) %>%
  sf::st_sfc() %>%
  sf::st_set_crs(3879)

distances <- centroids %>%
  sf::st_distance(p)

zones$value <- as.numeric(distances[, 1])

ggplot() +
  geom_sf(mapping = aes(fill = value),
          data = zones, color = NA) +
  scale_fill_distiller("Legendin otsikko", palette = "Greens") +
  geom_basemap() +
  coord_sf(
    xlim = c(bbox$xmin - 500, bbox$xmax + 500),
    ylim = c(bbox$ymin + 20000, bbox$ymax + 500),
    expand = FALSE,
    datum = sf::st_crs(3879)
  ) +
  # Title
  annotate(
    "text",
    x = bbox$xmin,
    y = bbox$ymax,
    hjust = 0,
    vjust = 1,
    label = "Mittarin otsikko",
    size = 10 / (72.27 / 25.4),
    colour = "#64BE1E"
  ) +
  # Subtitle
  annotate(
    "text",
    x = bbox$xmin,
    y = bbox$ymax - 4000,
    hjust = 0,
    vjust = 1,
    label = "Alaotsikko",
    size = 10 / (72.27 / 25.4),
    colour = "#333333"
  ) +
  # Caption
  annotate(
    "label",
    x = bbox$xmax,
    y = bbox$ymin + 20000,
    hjust = 1,
    vjust = 0,
    label = paste("Kuntajako: Maanmittauslaitos 2021",
                  "Aineiston nimi: Lisenssin antaja 20XX",
                  sep = "\n"),
    size = 10 / (72.27 / 25.4),
    color = "#333333",
    label.size = 0
  ) +
  theme_mal_map()

# A4: 210 × 297
# A5: 148 × 210
ggsave(
  here::here("Basemaps", "map.png"),
  width = 148,
  height = 169,
  units = "mm",
  dpi = 600
)
