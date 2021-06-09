# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones_2018.rds"))


# Plot --------------------------------------------------------------------

breaks <- quantile(zones$accessibility_wh, probs = seq(0, 1, 0.05), names = FALSE)
colors <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#000000")
nbreaks <- length(breaks)
values <- scales::rescale(
  x = seq(from = mean(breaks[c(1, 2)]),
          to = mean(breaks[c(nbreaks - 1, nbreaks)]),
          length.out = 5),
  to = c(0,1),
  from = range(zones$accessibility_wh)
)

ggplot() +
  geom_sf(mapping = aes(fill = accessibility_wh),
          data = zones, color = NA) +
  scale_fill_stepsn(
    name = "Henkilöä",
    breaks = breaks,
    colors = colors,
    guide = "colourbar",
    values = values
  ) +
  geom_basemap() +
  annotate_map(
    title = "Työvoimasaavutettavuus",
    subtitle = "helmet_4.0.4_2018_results"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_workforce-accessibility.png"))
