# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "squares.rds")) %>%
  dplyr::mutate(floor_area_diff = dplyr::if_else(!ensi, -floor_area_increase_2021_2040_ve0, floor_area_increase_2021_2040_ve0))

ensi <- readr::read_rds(here::here("results", "ensi.rds"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = floor_area_diff),
          data = results, color = NA) +
  geom_basemap() +
  geom_sf(mapping = aes(),
          data = ensi, color = "#333333", fill = NA) +
  scale_fill_fermenter(
    palette = "PiYG",
    direction = 1,
    name = "kerros-m2",
    labels = scales::label_number(accuracy = 1),
    breaks = c(-1000, -500, -250, -5, 5, 250, 500, 1000),
    limits = c(-1500, 1500),
    oob = scales::squish
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Asuntotuotannon kohdistuminen ensisijaisesti kehitettäville vyöhykkeille",
    subtitle = "2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_ensi-kem2_2040_ve0.png"))
