# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "zones_2018.rds"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = savu_zone),
          data = results, color = NA) +
  scale_fill_manual(
    name = "Saavutettavuus",
    values = c("#bd0026", "#f54026", "#fd9f44", "#fede80", "#ffffcc", "#bdbdbd", "#ffffff")
  ) +
  geom_basemap() +
  annotate_map(
    title = "SAVU-vyöhykkeet",
    subtitle = "helmet_4.0.4_2018_results"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_savu.png"))
