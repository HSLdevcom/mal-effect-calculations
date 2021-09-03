# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2018.rds")) %>%
  dplyr::select(zone, car_density) %>%
  dplyr::rename(car_density0 = car_density)


results1 <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, car_density) %>%
  dplyr::rename(car_density1 = car_density)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_car_density = car_density1 - car_density0)


# Plot --------------------------------------------------------------------

breaks <- seq(from = -225, to = 225, by = 50)
colors <- c("#3E8606", "#ffffff", "#f092cd")
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
  geom_sf(mapping = aes(fill = diff_car_density),
          data = results, color = NA) +
  scale_fill_stepsn(
    name = "autoa per 1000 asukasta",
    breaks = breaks,
    labels = scales::label_number(accuracy = 1),
    limits = limits,
    colors = colors,
    values = values,
    oob = scales::squish
  ) +
  geom_basemap() +
  annotate_map(
    title = "Muutos henkilöautotiheydessä",
    subtitle = "2018 Nykytila \U2192 2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_car-density_2020-2040_ve0.png"))
