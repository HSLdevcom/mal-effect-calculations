# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "2018")

accessibilities2 <- readr::read_rds(here::here("results", sprintf("centers2_%s.rds", config::get("scenario"))))
results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario")))) %>%
  dplyr::left_join(accessibilities2, by = c("zone" = "origin"))

breaks_car <- unique(quantile(results$car, probs = seq.int(0, 1, by = 1 / 5)))
breaks_transit <- unique(quantile(results$transit, probs = seq.int(0, 1, by = 1 / 5)))
breaks_bike <- unique(quantile(results$bike, probs = seq.int(0, 1, by = 1 / 5)))
breaks_walk <- unique(quantile(results$walk, probs = seq.int(0, 1, by = 1 / 5)))

results <- results %>%
  dplyr::mutate(
    car_bins = cut(car,
                   breaks = breaks_car,
                   include.lowest = TRUE),
    transit_bins = cut(transit,
                       breaks = breaks_transit,
                       include.lowest = TRUE),
    bike_bins = cut(bike,
                    breaks = breaks_bike,
                    include.lowest = TRUE),
    walk_bins = cut(walk,
                    breaks = breaks_walk,
                    include.lowest = TRUE))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = car_bins),
          data = results, color = NA) +
  scale_fill_viridis_d(
    option="plasma",
    name = "indeksi",
    direction = -1
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuus henkilöautolla",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility2_car_%s.png", config::get("scenario"))))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = transit_bins),
          data = results, color = NA) +
  scale_fill_viridis_d(
    option="plasma",
    name = "indeksi",
    direction = -1
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuus joukkoliikenteellä",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility2_transit_%s.png", config::get("scenario"))))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = bike_bins),
          data = results, color = NA) +
  scale_fill_viridis_d(
    option="plasma",
    name = "indeksi",
    direction = -1
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuus polkupyörällä",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility2_bike_%s.png", config::get("scenario"))))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = walk_bins),
          data = results, color = NA) +
  scale_fill_viridis_d(
    option="plasma",
    name = "indeksi",
    direction = -1
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuus kävellen",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility2_walk_%s.png", config::get("scenario"))))

# Data --------------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "2040_ve0")

accessibilities2 <- readr::read_rds(here::here("results", sprintf("centers2_%s.rds", config::get("scenario"))))
results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario")))) %>%
  dplyr::left_join(accessibilities2, by = c("zone" = "origin"))

results <- results %>%
  dplyr::mutate(
    car_bins = cut(car,
                   breaks = breaks_car,
                   include.lowest = TRUE),
    transit_bins = cut(transit,
                       breaks = breaks_transit,
                       include.lowest = TRUE),
    bike_bins = cut(bike,
                    breaks = breaks_bike,
                    include.lowest = TRUE),
    walk_bins = cut(walk,
                    breaks = breaks_walk,
                    include.lowest = TRUE))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = car_bins),
          data = results, color = NA) +
  scale_fill_viridis_d(
    option="plasma",
    name = "indeksi",
    direction = -1
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuus henkilöautolla",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility2_car_%s.png", config::get("scenario"))))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = transit_bins),
          data = results, color = NA) +
  scale_fill_viridis_d(
    option="plasma",
    name = "indeksi",
    direction = -1
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuus joukkoliikenteellä",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility2_transit_%s.png", config::get("scenario"))))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = bike_bins),
          data = results, color = NA) +
  scale_fill_viridis_d(
    option="plasma",
    name = "indeksi",
    direction = -1
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuus polkupyörällä",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility2_bike_%s.png", config::get("scenario"))))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = walk_bins),
          data = results, color = NA) +
  scale_fill_viridis_d(
    option="plasma",
    name = "indeksi",
    direction = -1
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kahden keskuksen matka-aikasaavutettavuus kävellen",
    subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_accessibility2_walk_%s.png", config::get("scenario"))))
