# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(omxr)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

zones <- here::here("results", "zones.rds") %>%
  readr::read_rds() %>%
  dplyr::rename(zone = SIJ2019)

demand_path <- here::here("data",
                          "helmet_4.0.4_2018_results",
                          "Matrices",
                          "demand_aht.omx")

demand_lookup <- demand_path %>%
  omxr::read_lookup(name = "zone_number")

demand <- demand_path %>%
  omxr::read_all_omx() %>%
  dplyr::select(origin, destination, bike_leisure, bike_work) %>%
  dplyr::mutate(origin = demand_lookup$Lookup[origin],
                destination = demand_lookup$Lookup[destination])

distance_lookup <- demand_path %>%
  omxr::read_lookup(name = "zone_number")

distance <- here::here("data",
                      "helmet_4.0.4_2018_results",
                      "Matrices",
                      "dist_aht.omx") %>%
  omxr::read_all_omx() %>%
  dplyr::select(origin, destination, bike) %>%
  dplyr::mutate(origin = distance_lookup$Lookup[origin],
                destination = distance_lookup$Lookup[destination])

matrices <- demand %>%
  dplyr::full_join(distance, by = c("origin", "destination")) %>%
  dplyr::filter(origin %in% zones$zone) %>%
  dplyr::filter(destination %in% zones$zone)

results <- matrices %>%
  dplyr::mutate(mileage = bike * (bike_work + bike_leisure)) %>%
  dplyr::group_by(origin) %>%
  dplyr::summarise(mileage = sum(mileage)) %>%
  dplyr::rename(zone = origin)

population <- here::here("data",
                         "Lahtodata",
                         "2016_zonedata",
                         "2017.pop") %>%
  readr::read_tsv(
    col_types = "iiddddd",
    comment = "#"
  ) %>%
  dplyr::rename(zone = X1) %>%
  dplyr::mutate(sh_7plus = rowSums(select(., starts_with("sh")))) %>%
  dplyr::mutate(population_7plus = total * sh_7plus) %>%
  dplyr::select(zone, population_7plus)

zones <- zones %>%
  dplyr::left_join(results, by = "zone") %>%
  dplyr::left_join(population, by = "zone") %>%
  dplyr::mutate(value = mileage / population_7plus) %>%
  dplyr::mutate(value = dplyr::if_else(dplyr::near(population_7plus, 0.0), 0.0, value))



# Plot --------------------------------------------------------------------

breaks <- quantile(zones$value, probs = seq(0, 1, 0.05), names = FALSE)
colors <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#000000")
nbreaks <- length(breaks)
values <- scales::rescale(
  x = seq(from = mean(breaks[c(1, 2)]),
          to = mean(breaks[c(nbreaks - 1, nbreaks)]),
          length.out = 5),
  to = c(0,1),
  from = range(zones$value)
)

ggplot() +
  geom_sf(mapping = aes(fill = value),
          data = zones, color = NA) +
  # scale_fill_stepsn(
  #   name = "Henkilöä",
  #   breaks = breaks,
  #   colors = colors,
  #   guide = "colourbar",
  #   values = values
  # ) +
  scale_fill_continuous(limits = c(0, 2.0)) +
  geom_basemap() +
  annotate_map(
    title = "Pyöräilymatkojen kilometrisuorite asukasta kohti",
    subtitle = "helmet_4.0.4_2018_results"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_mileage_bicycle.png"))
