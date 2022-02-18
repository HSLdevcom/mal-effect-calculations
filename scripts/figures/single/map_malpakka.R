# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))


# Plot --------------------------------------------------------------------

breaks <- c(0, 0.1, 0.4, 1, 2, 3, 5, 6)
limits <- range(breaks)
breaks <- breaks[c(-1, -length(breaks))]

ggplot() +
  geom_sf(mapping = aes(fill = malpakka),
          data = results, color = NA) +
  scale_fill_fermenter(
    palette = "YlGn",
    name = NULL,
    labels = scales::label_number(accuracy = 0.1, decimal.mark = ","),
    breaks = breaks,
    limits = limits,
    direction = 1,
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "MALPAKKA",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_malpakka_%s.png", scenario_attributes[["scenario"]])))
