# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))


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

plot_twocenters(results, bins_twocenters_car, title="Kahden keskuksen matka-aikasaavutettavuus henkilöautolla")
ggsave_map(here::here("figures", sprintf("map_twocenters_car_%s.png", scenario_attributes[["scenario"]])))
plot_twocenters(results, bins_twocenters_transit, title="Kahden keskuksen matka-aikasaavutettavuus joukkoliikenteellä")
ggsave_map(here::here("figures", sprintf("map_twocenters_transit_%s.png", scenario_attributes[["scenario"]])))
plot_twocenters(results, bins_twocenters_bike, title="Kahden keskuksen matka-aikasaavutettavuus polkupyörällä")
ggsave_map(here::here("figures", sprintf("map_twocenters_bike_%s.png", scenario_attributes[["scenario"]])))
plot_twocenters(results, bins_twocenters_walk, title="Kahden keskuksen matka-aikasaavutettavuus kävellen")
ggsave_map(here::here("figures", sprintf("map_twocenters_walk_%s.png", scenario_attributes[["scenario"]])))
plot_twocenters(results, bins_twocenters_all, title="Kahden keskuksen matka-aikasaavutettavuus kaikilla kulkutavoilla")
ggsave_map(here::here("figures", sprintf("map_twocenters_all_%s.png", scenario_attributes[["scenario"]])))
