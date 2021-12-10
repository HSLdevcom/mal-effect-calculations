# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "2018")

scale_and_squish <- function(x, xrange, a, b) {
  x_normal <- scale_to_range(x, xrange[1], xrange[2], a, b)
  x_normal_ab <- pmax(a, pmin(x_normal, b))
  return(x_normal_ab)
}

break_twocenters <- function(x) {
  unique(quantile(x, probs = seq.int(0, 1, by = 1 / 5)))
}

cut_twocenters <- function(x, breaks) {
  cut(x, breaks = breaks, include.lowest = TRUE)
}

ttime_to_bins <- function(x, xrange, a, b, breaks) {
  x_normal_ab <- scale_and_squish(x, xrange, a, b)
  return(cut_twocenters(x_normal_ab, breaks = breaks))
}

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))

# Dropping Suomenlinna because it distorts the results too much.
outlier_zones <- c(1531)

# Ranges are calculated in 2018 and used as such in 2040.
results_to_range <- results %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(!(zone %in% outlier_zones))
range_car <- range(results_to_range$ttime_twocenters_car)
range_transit <- range(results_to_range$ttime_twocenters_transit)
range_bike <- range(results_to_range$ttime_twocenters_bike)
range_walk <- range(results_to_range$ttime_twocenters_walk)

# Scaling to 1-100
a <- 1
b <- 100

results <- results %>%
  dplyr::select(zone, dplyr::starts_with("ttime_twocenters_")) %>%
  dplyr::mutate(
    ttime_twocenters_car_normal = scale_and_squish(ttime_twocenters_car, xrange = range_car, a = a, b = b),
    ttime_twocenters_transit_normal = scale_and_squish(ttime_twocenters_transit, xrange = range_transit, a = a, b = b),
    ttime_twocenters_bike_normal = scale_and_squish(ttime_twocenters_bike, xrange = range_bike, a = a, b = b),
    ttime_twocenters_walk_normal = scale_and_squish(ttime_twocenters_walk, xrange = range_walk, a = a, b = b),
  )

# Breaks are calculated in 2018 and used as such in 2040.
breaks_car <- break_twocenters(results$ttime_twocenters_car_normal)
breaks_transit <- break_twocenters(results$ttime_twocenters_transit_normal)
breaks_bike <- break_twocenters(results$ttime_twocenters_bike_normal)
breaks_walk <- break_twocenters(results$ttime_twocenters_walk_normal)

results <- results %>%
  dplyr::mutate(
    car_bins = cut_twocenters(ttime_twocenters_car_normal, breaks = breaks_car),
    transit_bins = cut_twocenters(ttime_twocenters_transit_normal, breaks = breaks_transit),
    bike_bins = cut_twocenters(ttime_twocenters_bike_normal, breaks = breaks_bike),
    walk_bins = cut_twocenters(ttime_twocenters_walk_normal, breaks = breaks_walk)
  )


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

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario")))) %>%
  dplyr::select(zone, dplyr::starts_with("ttime_twocenters_")) %>%
  dplyr::mutate(
    car_bins = ttime_to_bins(ttime_twocenters_car, xrange = range_car, a = a, b = b, breaks = breaks_car),
    transit_bins = ttime_to_bins(ttime_twocenters_transit, xrange = range_transit, a = a, b = b, breaks = breaks_transit),
    bike_bins = ttime_to_bins(ttime_twocenters_bike, xrange = range_bike, a = a, b = b, breaks = breaks_bike),
    walk_bins = ttime_to_bins(ttime_twocenters_walk, xrange = range_walk, a = a, b = b, breaks = breaks_walk),
  )


# Plot --------------------------------------------------------------------

plot_twocenters(results, car_bins, "Kahden keskuksen matka-aikasaavutettavuus henkilöautolla")
ggsave_map(here::here("figures", sprintf("map_twocenters_car_%s.png", config::get("scenario"))))
plot_twocenters(results, transit_bins, "Kahden keskuksen matka-aikasaavutettavuus joukkoliikenteellä")
ggsave_map(here::here("figures", sprintf("map_twocenters_transit_%s.png", config::get("scenario"))))
plot_twocenters(results, bike_bins, "Kahden keskuksen matka-aikasaavutettavuus polkupyörällä")
ggsave_map(here::here("figures", sprintf("map_twocenters_bike_%s.png", config::get("scenario"))))
plot_twocenters(results, walk_bins, "Kahden keskuksen matka-aikasaavutettavuus kävellen")
ggsave_map(here::here("figures", sprintf("map_twocenters_walk_%s.png", config::get("scenario"))))
