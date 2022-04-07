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
  # Hack to get NA values to legend in ggplot2 v3.3.5
  geom_sf(mapping = aes(color = ""),
          data = head(results, n = 1)) +
  geom_sf(mapping = aes(fill = accessibility_scaled),
          data = results, color = NA) +
  scale_fill_viridis(
    option = "magma",
    labels = scales::label_number(accuracy = 1),
    limits = c(0, 100),
    direction = -1,
    na.value = "#ffffff",
    oob = scales::squish
  ) +
  scale_colour_manual(values = NA) +
  guides(fill = guide_colorbar("indeksi",
                               order = 1),
         colour = guide_legend("Liian\nvähän\nasukkaita",
                               order = 99,
                               override.aes = list(color = "grey35",
                                                   fill = "#ffffff"))) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Saavutettavuus asukkaiden näkökulmasta",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility_%s.png", scenario_attributes[["scenario"]])))
