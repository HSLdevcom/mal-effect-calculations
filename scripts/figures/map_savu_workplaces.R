# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))


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
  coord_sf_mal() +
  annotate_map(
    title = "Työpaikkojen lukumäärä hyvillä ja heikoilla SAVU-vyöhykkeillä",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_savu_workplaces_%s.png", config::get("scenario"))))
