# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "zones_2018.rds"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = savu_goodness, alpha = total_wrk),
          data = results, color = NA) +
  scale_fill_manual(
    name = "Saavutettavuus",
    values = c("#4d9221", "#c51b7d")
  ) +
  scale_alpha_continuous(
    name = "Työpaikat",
    range = c(0, 1),
    limits = c(0, 4000),
    guide = guide_legend(override.aes = list(fill = "#646464"))) +
  geom_basemap() +
  annotate_map(
    title = "Työpaikkojen lukumäärä hyvillä ja heikoilla SAVU-vyöhykkeillä",
    subtitle = "helmet_4.0.4_2018_results"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_savu_workplaces.png"))
