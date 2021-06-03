# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))

results <- here::here("data",
                      "helmet_4.0.4_2018_results",
                      "origins_shares.txt") %>%
  readr::read_tsv(
    col_names = c("zone", "car", "transit", "bike", "walk"),
    col_types = "idddd",
    skip = 1
  ) %>%
  dplyr::mutate(value = bike + walk) %>%
  dplyr::select(zone, value)

zones <- zones %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::left_join(results, by = "zone")


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

ggplot() +
  geom_sf(mapping = aes(fill = value),
          data = zones, color = NA) +
  scale_fill_stepsn(
    name = "%",
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    breaks = breaks,
    limits = range(breaks),
    colors = colors,
    guide = "colourbar",
    values = values,
    oob = scales::squish
  ) +
  geom_basemap() +
  annotate_map(
    title = "Kävellen ja pyöräillen tehtyjen kiertomatkojen osuus alueelta alkavista kiertomatkoista",
    subtitle = "helmet_4.0.4_2018_results"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_mode-share_bike-walk.png"))
