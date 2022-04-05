# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("buffers-truck_all-vrk_%s.rds", scenario_attributes[["scenario"]]))) %>%
  # Arrange for improved plotting
  dplyr::arrange(truck_all_vrk)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(data = results, color = NA, fill = "#2a5475") +
  scale_volume(binwidth = 9000, height = 2000/8,
               labels = c("2 000", "4 000", "6 000", "8 000", "ajon./h"),
               x0 = unname(bbox$xmin),
               y0 = unname(bbox$ymax) - 12000) +
  coord_sf_mal() +
  annotate_map(
    title = "Tavaraliikenteen liikennemäärä vuorokaudessa",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_volumes-truck-all-vrk_%s.png", scenario_attributes[["scenario"]])))
