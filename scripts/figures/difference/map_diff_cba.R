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
  geom_sf(mapping = aes(fill = cba_transit_time_per_tour),
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
    title = "Joukkoliikenteen matka-aikamuutos kiertomatkaa kohden",
    subtitle = "2040 Vertailupohja \U2192 2040 1. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_cba_transit-time_tour_%s.png", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = cba_car_time_per_tour),
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
    title = "Henkilöauton matka-aikamuutos kiertomatkaa kohden",
    subtitle = "2040 Vertailupohja \U2192 2040 1. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_cba_car-time_tour_%s.png", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = cba_car_transit_time_per_tour),
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
    title = "Joukkoliikenteen ja henkilöauton matka-aikamuutos kiertomatkaa kohden",
    subtitle = "2040 Vertailupohja \U2192 2040 1. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_cba_car-transit-time_tour_%s.png", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = cba_transit_revenue / demand_transit),
          data = results, color = NA) +
  scale_fill_fermenter(
    palette = "RdBu",
    name = "euroa",
    limits = c(-1, 1),
    labels = scales::label_number(decimal_mark = ","),
    breaks = c(-0.75, -0.50, -0.25, -0.05, 0.05, 0.25, 0.50, 0.75),
    direction = -1,
    oob = scales::oob_squish_any
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Tulot joukkoliikenteestä kiertomatkaa kohden",
    subtitle = "2040 Vertailupohja \U2192 2040 1. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_cba_revenue_transit_tour_%s.png", scenario_attributes[["scenario"]])))