# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))


# Plot --------------------------------------------------------------------

breaks <- quantile(zones$workforce_accessibility, probs = seq(0, 1, 0.05), names = FALSE)
colors <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#000000")
nbreaks <- length(breaks)
values <- scales::rescale(
  x = seq(from = mean(breaks[c(1, 2)]),
          to = mean(breaks[c(nbreaks - 1, nbreaks)]),
          length.out = 5),
  to = c(0,1),
  from = range(zones$workforce_accessibility)
)

ggplot() +
  geom_sf(mapping = aes(fill = workforce_accessibility),
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
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_workforce-accessibility_%s.png", config::get("scenario"))))
