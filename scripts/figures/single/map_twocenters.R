# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "2018")

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))

# Dropping Suomenlinna because it distorts the results too much.
outlier_zones <- c(1531)

# Ranges are calculated in 2018 and used as such in 2040.
ranges <- results %>%
  sf::st_drop_geometry() %>%
  dplyr::summarise(
    car_min = min(ttime_twocenters_car[!(zone %in% outlier_zones)]),
    car_max = max(ttime_twocenters_car[!(zone %in% outlier_zones)]),
    transit_min = min(ttime_twocenters_transit[!(zone %in% outlier_zones)]),
    transit_max = max(ttime_twocenters_transit[!(zone %in% outlier_zones)]),
    bike_min = min(ttime_twocenters_bike[!(zone %in% outlier_zones)]),
    bike_max = max(ttime_twocenters_bike[!(zone %in% outlier_zones)]),
    walk_min = min(ttime_twocenters_walk[!(zone %in% outlier_zones)]),
    walk_max = max(ttime_twocenters_walk[!(zone %in% outlier_zones)]),
  ) %>%
  tidyr::pivot_longer(everything()) %>%
  tibble::deframe()

# Scaling to 1-100
a <- 1
b <- 100

results <- results %>%
  dplyr::select(zone, dplyr::starts_with("ttime_twocenters_")) %>%
  dplyr::mutate(
    ttime_twocenters_car_normal =  scale_to_range(ttime_twocenters_car, xmin = ranges["car_min"], xmax = ranges["car_max"], a = a, b = b),
    ttime_twocenters_transit_normal =  scale_to_range(ttime_twocenters_transit, xmin = ranges["transit_min"], xmax = ranges["transit_max"], a = a, b = b),
    ttime_twocenters_bike_normal =  scale_to_range(ttime_twocenters_bike, xmin = ranges["bike_min"], xmax = ranges["bike_max"], a = a, b = b),
    ttime_twocenters_walk_normal =  scale_to_range(ttime_twocenters_walk, xmin = ranges["walk_min"], xmax = ranges["walk_max"], a = a, b = b)
  ) %>%
  dplyr::mutate(
    ttime_twocenters_car_normal = pmax(1, pmin(ttime_twocenters_car_normal, 100)),
    ttime_twocenters_transit_normal = pmax(1, pmin(ttime_twocenters_transit_normal, 100)),
    ttime_twocenters_bike_normal = pmax(1, pmin(ttime_twocenters_bike_normal, 100)),
    ttime_twocenters_walk_normal = pmax(1, pmin(ttime_twocenters_walk_normal, 100)),
  )

# Breaks are calculated in 2018 and used as such in 2040.
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

plot_twocenters <- function(data, fill, title) {
  ggplot() +
    geom_sf(mapping = aes(fill = {{ fill }}),
            data = data, color = NA) +
    scale_fill_viridis_d(
      option="plasma",
      name = "indeksi",
      direction = -1
    ) +
    geom_basemap() +
    coord_sf_mal() +
    annotate_map(
      title = title,
      subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
    ) +
    theme_mal_map()
}

plot_twocenters(results, car_bins, "Kahden keskuksen matka-aikasaavutettavuus henkilöautolla")
ggsave_map(here::here("figures", sprintf("map_twocenters_car_%s.png", config::get("scenario"))))
plot_twocenters(results, transit_bins, "Kahden keskuksen matka-aikasaavutettavuus joukkoliikenteellä")
ggsave_map(here::here("figures", sprintf("map_twocenters_transit_%s.png", config::get("scenario"))))
plot_twocenters(results, bike_bins, "Kahden keskuksen matka-aikasaavutettavuus polkupyörällä")
ggsave_map(here::here("figures", sprintf("map_twocenters_bike_%s.png", config::get("scenario"))))
plot_twocenters(results, walk_bins, "Kahden keskuksen matka-aikasaavutettavuus kävellen")
ggsave_map(here::here("figures", sprintf("map_twocenters_walk_%s.png", config::get("scenario"))))


# Data --------------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "2040_ve0")

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))

results <- results %>%
  dplyr::select(zone, dplyr::starts_with("ttime_twocenters_")) %>%
  dplyr::mutate(
    ttime_twocenters_car_normal =  scale_to_range(ttime_twocenters_car, xmin = ranges["car_min"], xmax = ranges["car_max"], a = a, b = b),
    ttime_twocenters_transit_normal =  scale_to_range(ttime_twocenters_transit, xmin = ranges["transit_min"], xmax = ranges["transit_max"], a = a, b = b),
    ttime_twocenters_bike_normal =  scale_to_range(ttime_twocenters_bike, xmin = ranges["bike_min"], xmax = ranges["bike_max"], a = a, b = b),
    ttime_twocenters_walk_normal =  scale_to_range(ttime_twocenters_walk, xmin = ranges["walk_min"], xmax = ranges["walk_max"], a = a, b = b)
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

plot_twocenters(results, car_bins, "Kahden keskuksen matka-aikasaavutettavuus henkilöautolla")
ggsave_map(here::here("figures", sprintf("map_twocenters_car_%s.png", config::get("scenario"))))
plot_twocenters(results, transit_bins, "Kahden keskuksen matka-aikasaavutettavuus joukkoliikenteellä")
ggsave_map(here::here("figures", sprintf("map_twocenters_transit_%s.png", config::get("scenario"))))
plot_twocenters(results, bike_bins, "Kahden keskuksen matka-aikasaavutettavuus polkupyörällä")
ggsave_map(here::here("figures", sprintf("map_twocenters_bike_%s.png", config::get("scenario"))))
plot_twocenters(results, walk_bins, "Kahden keskuksen matka-aikasaavutettavuus kävellen")
ggsave_map(here::here("figures", sprintf("map_twocenters_walk_%s.png", config::get("scenario"))))
