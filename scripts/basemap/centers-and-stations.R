# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

centers_and_stations <- sf::read_sf(here::here("data", "centers_and_stations2.gpkg"))
readr::write_rds(centers_and_stations, file = here::here("results", "centers-and-stations.rds"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = center),
          data = centers_and_stations, color = NA) +
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

ggsave_map(here::here("figures", "map_centers-and-stations.png"))
