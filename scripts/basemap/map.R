# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

zones <- readr::read_rds(here::here("results", "zones.rds"))

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
  annotate_map(
    title = "Mittarin otsikko",
    subtitle = "Mittarin alaotsikko",
    caption = paste(c("Maastotietokanta: Maanmittauslaitos 2021",
                      "Aineiston nimi: Lisenssin antaja 20XX"),
                    collapse = "\n")
  ) +
  theme_mal_map()

# A4: 210 × 297
# A5: 148 × 210
ggsave(
  here::here("figures", "map.png"),
  width = 148,
  height = 169,
  units = "mm",
  dpi = 600
)
