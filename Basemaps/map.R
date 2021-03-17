# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

region <- readr::read_rds(here::here("Basemaps", "region.rds"))
bonus_region <- readr::read_rds(here::here("Basemaps", "bonus_region.rds"))
metro <- readr::read_rds(here::here("Basemaps", "metro.rds"))
train <- readr::read_rds(here::here("Basemaps", "train.rds"))
roads <- readr::read_rds(here::here("Basemaps", "roads.rds"))
municipalities <- readr::read_rds(here::here("Basemaps", "municipalities.rds"))
water <- readr::read_rds(here::here("Basemaps", "water.rds"))

zones <- readr::read_rds(here::here("Basemaps", "zones.rds"))

bbox <- sf::st_bbox(region)

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

  geom_sf(data = water, fill = "#a6cee3", color = NA, size = 0) +
  geom_sf(data = municipalities, color = "#9b3b6b", fill = NA, size = 0.26) +
  geom_sf(data = roads, color = "#64a5a5", size = 0.2) +

  geom_sf(data = train, color = "#000000", linetype = "solid", size = 0.66) +
  geom_sf(data = train, color = "#ffffff", linetype = "22", size = 0.53) +

  geom_sf(data = metro, color = "#ff7f00", linetype = "solid", size = 0.66) +
  geom_sf(data = metro, color = "#ffffff", linetype = "22", size = 0.53) +

  geom_sf(data = region, color = "#000000", fill = NA, size = 0.46) +
  geom_sf(data = bonus_region, color = "#000000", fill = NA, linetype = "33", size = 0.46) +

  coord_sf(
    xlim = c(bbox$xmin - 500, bbox$xmax + 500),
    ylim = c(bbox$ymin + 20000, bbox$ymax + 500),
    expand = FALSE,
    datum = sf::st_crs(3879)
  ) +
  labs(
    title = "Mittarin otsikko",
    subtitle = "Alaotsikko",
    caption = "Aineiston nimi: Lisenssin antaja 20XX"
  ) +
  theme_void() +
  theme(
    text = element_text(
      family = "sans",
      colour = "#333333",
      size = 10
    ),
    plot.title = element_text(colour = "#64BE1E"),
    panel.border = element_rect(colour = "#dddddc", fill = NA),
    panel.grid.major = element_line(colour = "#dddddc"),
    legend.position = c(0.00, 1.00),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(size = rel(1.0)),
    plot.caption = element_text(size = rel(1.0))
  )

# A4: 210 × 297
# A5: 148 × 210
ggsave(
  here::here("Basemaps", "map.png"),
  width = 148,
  height = 210,
  units = "mm",
  dpi = 600
)
