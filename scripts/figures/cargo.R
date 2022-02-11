# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Read data ---------------------------------------------------------------

demand_aht <- read_helmet_omx(file.path(config::get("helmet_data"),
                                        config::get("results"),
                                        "Matrices",
                                        "demand_aht.omx")) %>%
  dplyr::select(origin, destination, trailer_truck, truck)

dist_aht <- read_helmet_omx(file.path(config::get("helmet_data"),
                                      config::get("results"),
                                      "Matrices",
                                      "dist_aht.omx")) %>%
  dplyr::select(origin, destination, trailer_truck, truck)

ttimes_aht <- read_helmet_omx(file.path(config::get("helmet_data"),
                                        config::get("results"),
                                        "Matrices",
                                        "time_aht.omx")) %>%
  dplyr::select(origin, destination, trailer_truck, truck)


# Join data ---------------------------------------------------------------

helsinki_region <- 1:15499
# Terminal nodes inside Helsinki region
terminals <- c(31400, 31500, 31501, 31502, 31503)

matrices <- demand_aht %>%
  dplyr::left_join(dist_aht, by = c("origin", "destination"), suffix = c("", "_dist")) %>%
  dplyr::left_join(ttimes_aht, by = c("origin", "destination"), suffix = c("_demand", "_time")) %>%
  dplyr::filter(origin %in% c(helsinki_region, terminals) | destination %in% c(helsinki_region, terminals))

matrices <- matrices %>%
  dplyr::mutate(group = dplyr::case_when(
    origin %in% terminals | destination %in% terminals ~ "terminals",
    origin %in% helsinki_region & destination %in% helsinki_region ~ "internal",
    TRUE ~ "external"
  ))

vehicle_cost_truck <- 0.3189 # e/km
vehicle_cost_trailer_truck <- 0.6473 # e/km

capital_cost_truck <- 8.57 # e/h
capital_cost_trailer_truck <- 11.20 # e/h

travel_time_cost_truck <- 35.91 # e/h/auto
travel_time_cost_trailer_truck <- 40.22 # e/h/auto

matrices <- matrices %>%
  dplyr::mutate(
    trailer_truck_cost = (vehicle_cost_trailer_truck * trailer_truck_dist + capital_cost_trailer_truck / 60 * trailer_truck_time + travel_time_cost_trailer_truck / 60 * trailer_truck_time) * trailer_truck_demand,
    truck_cost = (vehicle_cost_truck * truck_dist + capital_cost_truck / 60 * truck_time + travel_time_cost_truck / 60 * truck_time) * truck_demand,
  )


# Aggregate data ----------------------------------------------------------

cargo <- matrices %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(cost = sum(trailer_truck_cost + truck_cost) / sum(trailer_truck_demand + truck_demand)) %>%
  dplyr::add_row(
    group = "all",
    cost = sum(matrices$trailer_truck_cost + matrices$truck_cost) / sum(matrices$trailer_truck_demand + matrices$truck_demand)
  )


# Translate data ----------------------------------------------------------

cargo <- cargo %>%
  dplyr::mutate(group = factor(group, levels = c("all", "terminals", "external", "internal"), labels = c("Helsingin seutu", "Terminaalikuljetukset", "Seudun maarajan ylittävät kuljetukset", "Seudun sisäiset kuljetukset"))) %>%
  dplyr::arrange(group)


# Output ------------------------------------------------------------------

readr::write_rds(cargo, file = here::here("results", sprintf("cargo_%s.rds", config::get("scenario"))))
