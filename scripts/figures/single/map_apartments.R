# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]]))) %>%
  dplyr::mutate(apartments = 1 - detach)


# Plot ------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = apartments),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "OrRd",
    name = "%",
    labels = scales::label_percent(accuracy = 1, suffix = " %"),
    limits = c(0, 1),
    direction = 1,
    oob = scales::squish
  ) +
  scale_color_manual(
    values = NA
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", size = 0.1) +
  coord_sf_mal() +
  annotate_map(
    title = "Kerrostaloasuntojen osuus",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_apartments_%s.png", scenario_attributes[["scenario"]])))
