# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("buffers_%s.rds", scenario_attributes[["scenario"]]))) %>%
  # Arrange for improved plotting
  dplyr::arrange(-relative_speed)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(mapping = aes(fill = relative_speed),
          data = results, color = NA) +
  scale_fill_stepsn(
    colours = c("#000000", "#ff0000", "#ffa94c", "#ffff08", "#7fde40"),
    values = seq(0.1, 0.9, 0.2),
    labels = scales::label_percent(accuracy = 1, suffix = " %"),
    name = NULL,
    breaks = seq(0.2, 0.8, by = 0.2),
    limits = c(0, 1),
    oob = scales::squish
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Aamuhuipputunnin ajonopeus suhteessa vapaaseen ajonopeuteen",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_relative-speed_%s.png", scenario_attributes[["scenario"]])))
