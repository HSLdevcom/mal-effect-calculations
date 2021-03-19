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
    datum = sf::st_crs(3879)) +
  annotate_map(
    title = "Mittarin otsikko",
    subtitle = "Mittarin alaotsikko",
    caption = "Kuntajako: Maanmittauslaitos 2021\nAineiston nimi: Lisenssin antaja 20XX"
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
