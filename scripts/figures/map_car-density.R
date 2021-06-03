# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))

results <- here::here("data",
                      "helmet_4.0.4_2018_results",
                      "car_density.txt") %>%
  readr::read_tsv(
    col_names = c("zone", "value"),
    col_types = "id",
    skip = 1
  ) %>%
  dplyr::mutate(value = 1000 * value)

zones <- zones %>%
  dplyr::rename(zone = SIJ2019) %>%
  dplyr::left_join(results, by = "zone")


# Plot --------------------------------------------------------------------

breaks <- c(100, 200, 300, 400, 500, 600, 700.1)
colors <- c("#004100", "#007300", "#00b300", "#98e87e", "#e5ff90", "#ffff78")
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
    name = "autoa per 1000 asukasta",
    labels = scales::label_number(accuracy = 1),
    breaks = breaks,
    limits = range(breaks),
    colors = colors,
    guide = "colourbar",
    values = values,
    oob = scales::squish
  ) +
  geom_basemap() +
  annotate_map(
    title = "Henkilöautotiheys",
    subtitle = "helmet_4.0.4_2018_results"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_car-density.png"))
