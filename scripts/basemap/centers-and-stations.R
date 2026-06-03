# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

centers_and_stations <- sf::read_sf(here::here("data", "centers_and_stations2.gpkg"))
readr::write_rds(centers_and_stations, file = here::here("results", "centers-and-stations.rds"))

# Update columns ----------------------------------------------------------

centers_and_stations <- centers_and_stations %>%
  dplyr::rename(station_2023 = station_2018) %>%
  dplyr::rename(station_2045_ve0 = station_2040_ve0) %>%
  dplyr::rename(station_2045_ve1 = station_2040_ve1) %>%
  dplyr::rename(station_2045_ve2 = station_2040_ve2) %>%
  dplyr::rename(station_2045_suunnitelma = station_2040_suunnitelma)

# Plot --------------------------------------------------------------------

centers_and_stations_ve1 <- centers_and_stations %>%
  dplyr::filter(center | station_2045_ve1)
centers_and_stations_ve2 <- centers_and_stations %>%
  dplyr::filter(center | station_2045_ve2)
centers_and_stations_suunnitelma <- centers_and_stations %>%
  dplyr::filter(center | station_2045_suunnitelma)

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
    subtitle = "2045 1. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_basemap_centers-and-stations_2045_ve1.png"))

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
    subtitle = "2045 2. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_basemap_centers-and-stations_2045_ve2.png"))

ggplot() +
  geom_sf(mapping = aes(fill = center),
          data = centers_and_stations_suunnitelma, color = NA) +
  geom_basemap() +
  scale_fill_manual("Alue",
                    values = c("#BFD7AC", "#3E8606"),
                    labels = c("Raskas raideliikenne", "Seudun keskus")) +
  coord_sf_mal() +
  annotate_map(
    title = "Seudun keskukset ja raskas raideliikenne",
    subtitle = "2045 Suunnitelma"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_basemap_centers-and-stations_2045_suunnitelma.png"))
