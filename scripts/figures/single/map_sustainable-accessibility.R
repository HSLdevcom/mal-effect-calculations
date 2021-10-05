# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))


# Plot --------------------------------------------------------------------

breaks <- seq(from = -185, to = -135, by = 5)
limits <- range(breaks)
breaks <- breaks[c(-1, -length(breaks))]

ggplot() +
  geom_sf(mapping = aes(fill = sustainable_accessibility),
          data = results, color = NA) +
  scale_fill_viridis_b(
    option="magma",
    name = NULL,
    labels = scales::label_number(accuracy = 1),
    breaks = breaks,
    limits = limits,
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Saavutettavuus asukkaiden näkökulmasta",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_sustainable-accessibility_%s.png", config::get("scenario"))))
