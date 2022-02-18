# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

# Let us handle CO2 emissions separately. They are only reported for whole
# Helsinki region, not areas.


# Read data ---------------------------------------------------------------

buses <- tibble::tribble(
  ~transit_vehicle, ~vehicle,
  "ValluVakio",     "bus_other",
  "ValluPika",      "bus_other",
  "HSL-bussi",      "bus_hsl",
  "HSL-runkob",     "bus_hsl"
)

# This data is only for total Helsinki region
transit_kms <- file.path(config::get("helmet_data"),
                         scenario_attributes[["results"]],
                         "transit_kms.txt") %>%
  read_tsv(skip = 1, # skip old column names
           col_types = "cdd",
           col_names = c("transit_vehicle", "dist", "time"),
           na = "nan") %>%
  dplyr::semi_join(buses, by = "transit_vehicle") %>%
  dplyr::left_join(buses, by = "transit_vehicle") %>%
  dplyr::group_by(vehicle) %>%
  dplyr::summarise(dist = sum(dist))

kms <- readr::read_rds(here::here("results", sprintf("areas_%s.rds", scenario_attributes[["scenario"]]))) %>%
  dplyr::filter(area %in% "Helsingin seutu") %>%
  dplyr::select(vehicle_kms_car, vehicle_kms_van, vehicle_kms_truck_all) %>%
  tidyr::pivot_longer(
    cols = starts_with("vehicle_kms_"),
    names_to = "vehicle",
    names_prefix = "vehicle_kms_",
    values_to = "dist"
  ) %>%
  dplyr::bind_rows(transit_kms)

co2 <- here::here("utilities", "co2.tsv") %>%
  readr::read_tsv(col_types = "cid") %>%
  dplyr::filter(year == config::get("co2_year")) %>%
  dplyr::select(vehicle, co2)

emissions <- kms %>%
  dplyr::left_join(co2, by = "vehicle") %>%
  dplyr::mutate(emission = dist * co2 * 300)

if (config::get("scenario") == config::get("baseline_scenario")) {
  emission_statistics <- here::here("utilities", "co2_statistics.tsv") %>%
    readr::read_tsv(col_types = "id") %>%
    tibble::deframe()
  correction <- emission_statistics["2018"] / sum(emissions$emission)
  message(sprintf("Correction factor for baseline emissions is %.3f.", correction))
  readr::write_rds(correction, file = here::here("results", "emission_correction.rds"))
} else {
  correction <- readr::read_rds(here::here("results", "emission_correction.rds"))
}

emissions <- emissions %>%
  dplyr::mutate(emission = correction * emission) %>%
  dplyr::add_row(
    vehicle = "total",
    emission = sum(.$emission)
  )

translations <- here::here("utilities", "vehicles.tsv") %>%
  readr::read_tsv(col_types = "cc")

emissions <- emissions %>%
  dplyr::mutate(vehicle = factor(vehicle, levels = translations$level, labels = translations$label))


# Output ------------------------------------------------------------------

readr::write_rds(emissions, file = here::here("results", sprintf("emissions_%s.rds", scenario_attributes[["scenario"]])))
