# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("buffers_%s.rds", scenario_attributes[["scenario"]]))) %>%
  # Arrange for improved plotting
  dplyr::arrange(volume_aht)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_basemap(show_roads = FALSE) +
  geom_sf(mapping = aes(fill = volume_aht),
          data = results, color = NA) +
  scale_fill_viridis_b(
    option = "inferno",
    name = "ajon. / h",
    breaks = seq(1000, 5000, by = 1000),
    limits = c(0, 6000),
    direction = -1,
    oob = scales::squish
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Moottoriajoneuvoliikenteen liikennemäärä aamuhuipputuntina",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_volumes-aht_%s.png", scenario_attributes[["scenario"]])))
