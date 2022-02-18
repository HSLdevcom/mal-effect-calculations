# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = cba_transit_time_per_person),
          data = results, color = NA) +
  scale_fill_fermenter(
    palette = "PRGn",
    direction = -1,
    name = "minuuttia",
    breaks = c(-5, -3, -1, 1, 3, 5),
    limits = c(-7, 7),
    labels = scales::label_number(accuracy = 1),
    oob = scales::oob_squish_any
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Joukkoliikenteen matka-aikasäästö asukasta kohden",
    subtitle = "2040 Vertailupohja \U2192 2040 Varjo"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_cba_transit-time_%s.png", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = cba_car_time_per_person),
          data = results, color = NA) +
  scale_fill_fermenter(
    palette = "PRGn",
    direction = -1,
    name = "minuuttia",
    breaks = c(-5, -3, -1, 1, 3, 5),
    limits = c(-7, 7),
    labels = scales::label_number(accuracy = 1),
    oob = scales::oob_squish_any
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Henkilöauton matka-aikasäästö asukasta kohden",
    subtitle = "2040 Vertailupohja \U2192 2040 Varjo"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_cba_car-time_%s.png", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = cba_transit_time_per_person + cba_car_time_per_person),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "PRGn",
    direction = -1,
    name = "minuuttia",
    limits = c(-15, 15),
    labels = scales::label_number(accuracy = 1),
    oob = scales::oob_squish_any
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Joukkoliikenteen ja henkilöauton yhteenlaskettu matka-aikasäästö asukasta kohden",
    subtitle = "2040 Vertailupohja \U2192 2040 Varjo"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_cba_car-transit-time_%s.png", scenario_attributes[["scenario"]])))
