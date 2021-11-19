# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("buffers_%s.rds", config::get("scenario")))) %>%
  # Arrange for improved plotting
  dplyr::arrange(-relative_speed)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap() +
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
    title = "Aamuhuipputunnin ajonopeus suhteessa päivätunnin ajonopeuteen",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_relative-speed_%s.png", config::get("scenario"))))
