# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = workplace_accessibility),
          data = results, color = NA) +
  scale_fill_viridis_c(
    option="viridis",
    name = "indeksi",
    labels = scales::label_number(accuracy = 1),
    limits = c(0, 100),
    direction = -1,
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Työpaikkojen kasautuminen",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_workplace-accessibility_%s.png", scenario_attributes[["scenario"]])))
