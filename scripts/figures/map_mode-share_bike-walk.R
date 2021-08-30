# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))


# Plot --------------------------------------------------------------------

breaks <- seq(from = 0.2, to = 0.60001, by = 0.05)
colors <- c("#ffffff", "#64BE1E")
nbreaks <- length(breaks)
values <- scales::rescale(
  x = seq(from = mean(breaks[c(1, 2)]),
          to = mean(breaks[c(nbreaks - 1, nbreaks)]),
          length.out = length(colors)),
  to = c(0,1),
  from = range(breaks)
)
limits <- range(breaks) + c(-0.0001, 0.0001)
breaks <- breaks[c(-1, -length(breaks))]

ggplot() +
  geom_sf(mapping = aes(fill = mode_share_bike_walk),
          data = results, color = NA) +
  scale_fill_stepsn(
    name = "%",
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    breaks = breaks,
    limits = limits,
    colors = colors,
    values = values,
    oob = scales::squish
  ) +
  geom_basemap() +
  annotate_map(
    title = "Kävellen ja pyöräillen tehtyjen kiertomatkojen osuus alueelta alkavista kiertomatkoista",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_mode-share_bike-walk_%s.png", config::get("scenario"))))
