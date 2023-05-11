# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

centers_and_stations <- sf::read_sf(here::here("data", "centers_and_stations2.gpkg"))
readr::write_rds(centers_and_stations, file = here::here("results", "centers-and-stations.rds"))


# Plot --------------------------------------------------------------------

centers_and_stations_ve1 <- centers_and_stations %>%
  dplyr::filter(center | station_2040_ve1)
centers_and_stations_ve2 <- centers_and_stations %>%
  dplyr::filter(center | station_2040_ve2)

ggplot() +
  geom_sf(mapping = aes(fill = center),
          data = centers_and_stations_ve1, color = NA) +
  geom_basemap() +
  scale_fill_manual("Alue",
                    values = c("#BFD7AC", "#3E8606"),
                    labels = c("Raskas raideliikenne", "Seudun keskus")) +
  coord_sf_mal() +
  annotate_map(
    title = "Seudun keskukset ja raskas raideliikenne",
    subtitle = "2040 1. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_basemap_centers-and-stations_2040_ve1.png"))

ggplot() +
  geom_sf(mapping = aes(fill = center),
          data = centers_and_stations_ve2, color = NA) +
  geom_basemap() +
  scale_fill_manual("Alue",
                    values = c("#BFD7AC", "#3E8606"),
                    labels = c("Raskas raideliikenne", "Seudun keskus")) +
  coord_sf_mal() +
  annotate_map(
    title = "Seudun keskukset ja raskas raideliikenne",
    subtitle = "2040 2. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_basemap_centers-and-stations_2040_ve2.png"))
