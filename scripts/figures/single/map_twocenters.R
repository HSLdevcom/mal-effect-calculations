# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "2018")

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))

ranges <- results %>%
  sf::st_drop_geometry() %>%
  dplyr::summarise(
    car_min = min(ttime_twocenters_car[zone != 1531]),
    car_max = max(ttime_twocenters_car[zone != 1531]),
    transit_min = min(ttime_twocenters_transit[zone != 1531]),
    transit_max = max(ttime_twocenters_transit[zone != 1531]),
    bike_min = min(ttime_twocenters_bike[zone != 1531]),
    bike_max = max(ttime_twocenters_bike[zone != 1531]),
    walk_min = min(ttime_twocenters_walk[zone != 1531]),
    walk_max = max(ttime_twocenters_walk[zone != 1531]),
  ) %>%
  tidyr::pivot_longer(everything()) %>%
  tibble::deframe()

# Scaling to 1-100
a <- 1
b <- 100

results <- results %>%
  dplyr::select(zone, dplyr::starts_with("ttime_twocenters_")) %>%
  dplyr::mutate(
    ttime_twocenters_car_normal =  (b - a) * (ttime_twocenters_car - ranges["car_min"]) / (ranges["car_max"] - ranges["car_min"]) + a,
    ttime_twocenters_transit_normal =  (b - a) * (ttime_twocenters_transit - ranges["transit_min"]) / (ranges["transit_max"] - ranges["transit_min"]) + a,
    ttime_twocenters_bike_normal =  (b - a) * (ttime_twocenters_bike - ranges["bike_min"]) / (ranges["bike_max"] - ranges["bike_min"]) + a,
    ttime_twocenters_walk_normal =  (b - a) * (ttime_twocenters_walk - ranges["walk_min"]) / (ranges["walk_max"] - ranges["walk_min"]) + a
  ) %>%
  dplyr::mutate(
    ttime_twocenters_car_normal = pmax(1, pmin(ttime_twocenters_car_normal, 100)),
    ttime_twocenters_transit_normal = pmax(1, pmin(ttime_twocenters_transit_normal, 100)),
    ttime_twocenters_bike_normal = pmax(1, pmin(ttime_twocenters_bike_normal, 100)),
    ttime_twocenters_walk_normal = pmax(1, pmin(ttime_twocenters_walk_normal, 100)),
  )

breaks_car <- unique(quantile(results$ttime_twocenters_car_normal, probs = seq.int(0, 1, by = 1 / 5)))
breaks_transit <- unique(quantile(results$ttime_twocenters_transit_normal, probs = seq.int(0, 1, by = 1 / 5)))
breaks_bike <- unique(quantile(results$ttime_twocenters_bike_normal, probs = seq.int(0, 1, by = 1 / 5)))
breaks_walk <- unique(quantile(results$ttime_twocenters_walk_normal, probs = seq.int(0, 1, by = 1 / 5)))

results <- results %>%
  dplyr::mutate(
    car_bins = cut(ttime_twocenters_car_normal,
                   breaks = breaks_car,
                   include.lowest = TRUE),
    transit_bins = cut(ttime_twocenters_transit_normal,
                       breaks = breaks_transit,
                       include.lowest = TRUE),
    bike_bins = cut(ttime_twocenters_bike_normal,
                    breaks = breaks_bike,
                    include.lowest = TRUE),
    walk_bins = cut(ttime_twocenters_walk_normal,
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

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))

results <- results %>%
  dplyr::select(zone, dplyr::starts_with("ttime_twocenters_")) %>%
  dplyr::mutate(
    ttime_twocenters_car_normal =  (b - a) * (ttime_twocenters_car - ranges["car_min"]) / (ranges["car_max"] - ranges["car_min"]) + a,
    ttime_twocenters_transit_normal =  (b - a) * (ttime_twocenters_transit - ranges["transit_min"]) / (ranges["transit_max"] - ranges["transit_min"]) + a,
    ttime_twocenters_bike_normal =  (b - a) * (ttime_twocenters_bike - ranges["bike_min"]) / (ranges["bike_max"] - ranges["bike_min"]) + a,
    ttime_twocenters_walk_normal =  (b - a) * (ttime_twocenters_walk - ranges["walk_min"]) / (ranges["walk_max"] - ranges["walk_min"]) + a
  ) %>%
  dplyr::mutate(
    ttime_twocenters_car_normal = pmax(1, pmin(ttime_twocenters_car_normal, 100)),
    ttime_twocenters_transit_normal = pmax(1, pmin(ttime_twocenters_transit_normal, 100)),
    ttime_twocenters_bike_normal = pmax(1, pmin(ttime_twocenters_bike_normal, 100)),
    ttime_twocenters_walk_normal = pmax(1, pmin(ttime_twocenters_walk_normal, 100)),
  )

results <- results %>%
  dplyr::mutate(
    car_bins = cut(ttime_twocenters_car_normal,
                   breaks = breaks_car,
                   include.lowest = TRUE),
    transit_bins = cut(ttime_twocenters_transit_normal,
                       breaks = breaks_transit,
                       include.lowest = TRUE),
    bike_bins = cut(ttime_twocenters_bike_normal,
                    breaks = breaks_bike,
                    include.lowest = TRUE),
    walk_bins = cut(ttime_twocenters_walk_normal,
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
