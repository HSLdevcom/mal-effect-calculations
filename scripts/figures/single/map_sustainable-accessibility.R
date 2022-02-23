# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = sustainable_accessibility_scaled),
          data = results, color = NA) +
  scale_fill_viridis(
    option = "magma",
    name = "indeksi",
    labels = scales::label_number(accuracy = 1),
    limits = c(0, 100),
    direction = -1,
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Saavutettavuus asukkaiden näkökulmasta",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_sustainable-accessibility_%s.png", scenario_attributes[["scenario"]])))
