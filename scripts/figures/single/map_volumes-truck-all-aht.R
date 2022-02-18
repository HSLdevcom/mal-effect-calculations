# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("buffers-truck_all_%s.rds", scenario_attributes[["scenario"]]))) %>%
  # Arrange for improved plotting
  dplyr::arrange(truck_all_aht)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(#mapping = aes(fill = truck_all_aht),
    data = results, color = NA, fill = "#2a5475") +
  scale_volume(binwidth = 7000, height = 300,
               labels = c("100", "200", "300", "400", "500", "ajon./h"),
               x0 = unname(bbox$xmin),
               y0 = unname(bbox$ymax) - 12000) +
  coord_sf_mal() +
  annotate_map(
    title = "Tavaraliikenteen liikennemäärä aamuhuipputuntina",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_volumes-truck-all-aht_%s.png", scenario_attributes[["scenario"]])))
