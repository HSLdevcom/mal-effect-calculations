# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Read data ---------------------------------------------------------------

translations <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc")

co2 <- here::here("utilities", "co2.tsv") %>%
  readr::read_tsv(col_types = "cid")

car_density <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "car_density_areas.txt"),
  col_types = "cd",
  first_col_name = "area"
)
car_use <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "car_use_areas.txt"),
  col_types = "cd",
  first_col_name = "area"
)
origin_demand <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "origin_demand_areas.txt"),
  col_types = "cdddd",
  first_col_name = "area"
)
own_zone_demand <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "own_zone_demand.txt"),
  col_types = "cdddddddddddddddddddddddddddddddddddd",
  first_col_name = "area"
)
vehicle_kms_modes <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "vehicle_kms_areas.txt"),
  col_types = "cddddddddd",
  first_col_name = "area"
)
workforce_accessibility <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "workforce_accessibility_per_area.txt"),
  col_types = "cd",
  first_col_name = "area"
)

noise <- read_tsv(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "noise_areas.txt"),
  skip = 1, # skip old column names
  col_types = "cdd",
  col_names = c("area", "noise_area_km2", "noise_population") # override column names
)
aggregated_demand <- read_tsv(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "aggregated_demand.txt"),
  col_types = "ccccd",
  col_names = c("area_origin", "area_destination", "purpose", "mode", "demand"),
  na = "nan"
)


# Read Helsinki region data -----------------------------------------------

transit_kms <- read_tsv(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "transit_kms.txt"),
  skip = 1, # skip old column names
  col_types = "cdd",
  col_names = c("transit_vehicle", "dist", "time"),
  na = "nan"
)


# Read and aggregate zone data --------------------------------------------

zones <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario")))) %>%
  sf::st_drop_geometry()

zones1 <- zones %>%
  dplyr::group_by(area) %>%
  dplyr::summarise(
    sustainable_accessibility = weighted.mean(sustainable_accessibility, total_pop),
    total_pop = sum(total_pop),
    total_wrk = sum(total_wrk)
  )

zones2 <- zones %>%
  dplyr::group_by(area, savu_goodness, .drop = FALSE) %>%
  dplyr::summarise(
    goodness_wrk = sum(total_wrk),
    .groups = "drop"
  ) %>%
  dplyr::filter(savu_goodness == "SAVU hyvä")


# Join data ---------------------------------------------------------------

# Rename columns to avoid name collisions
vehicle_kms_modes <- vehicle_kms_modes %>%
  dplyr::rename_with(~ paste0("vehicle_kms_", .x), -area)
workforce_accessibility <- workforce_accessibility %>%
  dplyr::rename(workforce_accessibility = wh)
origin_demand <- origin_demand %>%
  dplyr::rename_with(~ paste0("origin_demand_", .x), -area)

areas <- data.frame(area = unique(zones$area)) %>%
  dplyr::left_join(zones1, by = "area") %>%
  dplyr::left_join(zones2, by = "area") %>%
  dplyr::left_join(vehicle_kms_modes, by = "area") %>%
  dplyr::left_join(workforce_accessibility, by = "area") %>%
  dplyr::left_join(origin_demand, by = "area") %>%
  dplyr::left_join(car_density, by = "area") %>%
  dplyr::left_join(noise, by = "area")


# Impact assessment columns  ----------------------------------------------

areas <- areas %>%
  dplyr::mutate(vehicle_kms_total = rowSums(select(., vehicle_kms_car_work, vehicle_kms_car_leisure, vehicle_kms_trailer_truck, vehicle_kms_truck, vehicle_kms_van, vehicle_kms_bus))) %>%
  dplyr::mutate(
    co2_car = NA_real_,
    co2_van = NA_real_,
    co2_bus_hsl = NA_real_,
    co2_bus_other = NA_real_,
    co2_truck_all = NA_real_,
  ) %>%
  dplyr::mutate(car_density = 1000 * car_density) %>%
  dplyr::mutate(goodness_share = goodness_wrk / total_wrk) %>%
  dplyr::mutate(
    origin_demand_total = origin_demand_transit + origin_demand_car + origin_demand_walk + origin_demand_bike,
    origin_share_transit = origin_demand_transit / origin_demand_total,
    origin_share_car = origin_demand_car / origin_demand_total,
    origin_share_walk = origin_demand_walk / origin_demand_total,
    origin_share_bike = origin_demand_bike / origin_demand_total,
  )

# This data is only for total Helsinki region
buses <- tibble::tribble(
  ~transit_vehicle, ~vehicle,
  "ValluVakio",     "bus_other",
  "ValluPika",      "bus_other",
  "HSL-bussi",      "bus_hsl",
  "HSL-runkob",     "bus_hsl"
)

transit_kms <- transit_kms %>%
  dplyr::filter(transit_vehicle %in% c("ValluVakio", "ValluPika", "HSL-bussi", "HSL-runkob")) %>%
  dplyr::left_join(buses, by = "transit_vehicle") %>%
  dplyr::group_by(vehicle) %>%
  dplyr::summarise(dist = sum(dist))

vehicle_kms_bus_hsl <- transit_kms$dist[transit_kms$vehicle == "bus_hsl"]
vehicle_kms_bus_other <- transit_kms$dist[transit_kms$vehicle == "bus_other"]

co2 <- co2 %>%
  dplyr::filter(year == config::get("co2_year")) %>%
  dplyr::select(vehicle, co2) %>%
  tibble::deframe()


# Add total row -----------------------------------------------------------

areas <- areas %>%
  dplyr::add_row(
    area = "helsinki_region",
    co2_car = co2["car"] * (sum(.$vehicle_kms_car_work) + sum(.$vehicle_kms_car_leisure)),
    co2_van = co2["van"] * sum(.$vehicle_kms_van),
    co2_bus_hsl = co2["bus_hsl"] * vehicle_kms_bus_hsl,
    co2_bus_other = co2["bus_other"] * vehicle_kms_bus_other,
    co2_truck_all = co2["truck_all"] * (sum(.$vehicle_kms_trailer_truck) + sum(.$vehicle_kms_truck)),
    sustainable_accessibility = weighted.mean(.$sustainable_accessibility, .$total_pop),
    workforce_accessibility = weighted.mean(.$workforce_accessibility, .$total_wrk),
    car_density = weighted.mean(.$car_density, .$total_pop),
    goodness_share = sum(.$goodness_wrk) / sum(.$total_wrk),
    origin_share_walk = sum(.$origin_demand_walk) / sum(.$origin_demand_total),
    origin_share_transit = sum(.$origin_demand_transit) / sum(.$origin_demand_total),
    origin_share_car = sum(.$origin_demand_car) / sum(.$origin_demand_total),
    origin_share_bike = sum(.$origin_demand_bike) / sum(.$origin_demand_total),
    noise_area_km2 = sum(.$noise_area_km2),
    noise_population = sum(.$noise_population),
    total_pop = sum(.$total_pop),
    total_wrk = sum(.$total_wrk)
  )


# Translate data ----------------------------------------------------------

areas <- areas %>%
  dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label)) %>%
  dplyr::arrange(area)


# Output ------------------------------------------------------------------

readr::write_rds(areas, file = here::here("results", sprintf("areas_%s.rds", config::get("scenario"))))
