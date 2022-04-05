# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("buffers-car-vrk_%s.rds", scenario_attributes[["scenario"]]))) %>%
  # Arrange for improved plotting
  dplyr::arrange(car_vrk)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(mapping = aes(fill = car_vrk),
          data = results, color = NA) +
  scale_fill_viridis_b(
    option = "inferno",
    name = "ajon.",
    labels = scales::label_number(),
    breaks = seq(10000, 50000, by = 10000),
    limits = c(0, 60000),
    direction = -1,
    oob = scales::squish
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Henkilöautoliikenteen liikennemäärä vuorokaudessa",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_volumes-car-vrk_%s.png", scenario_attributes[["scenario"]])))
