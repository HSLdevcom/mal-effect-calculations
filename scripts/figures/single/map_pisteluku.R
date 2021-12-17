# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario")))) %>%
  dplyr::mutate(sustainable_accessibility = -sustainable_accessibility) %>%
  dplyr::arrange(sustainable_accessibility)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = sustainable_accessibility),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "Spectral",
    name = "Pisteluku",
    labels = scales::label_number(accuracy = 1),
    direction = 1,
    limits = c(-185.9973, -122.3190),
    oob = scales::squish
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", size = 0.1) +
  coord_sf_mal() +
  annotate_map(
    title = "Pisteluku",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_pisteluku_%s.png", config::get("scenario"))))