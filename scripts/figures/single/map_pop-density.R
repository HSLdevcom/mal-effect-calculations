# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]]))) %>%
  dplyr::mutate(pop_density = total_pop / land_area)


# Plot ------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = pop_density),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "YlGn",
    name = "asukasta per km2",
    labels = scales::label_number(accuracy = 1),
    direction = 1,
    limits = c(0, 10000),
    oob = scales::squish
  ) +
  scale_color_manual(
    values = NA
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", size = 0.1) +
  coord_sf_mal() +
  annotate_map(
    title = "Väestötiheys",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_pop-density_%s.png", scenario_attributes[["scenario"]])))
