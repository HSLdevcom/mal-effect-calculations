# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]]))) %>%
  dplyr::mutate(wrk_density = total_wrk / land_area)


# Plot ------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = wrk_density),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "OrRd",
    name = "työpaikkaa per km2",
    labels = scales::label_number(accuracy = 1),
    direction = 1,
    limits = c(0, 10000),
    oob = scales::squish
  ) +
  scale_color_manual(
    values = NA
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", linewidth = 0.05) +
  coord_sf_mal() +
  annotate_map(
    title = "Työpaikkatiheys",
    subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_wrk-density_%s.png", scenario_attributes[["scenario"]])))
