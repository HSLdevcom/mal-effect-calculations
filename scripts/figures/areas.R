# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

read_tsv_helmet <- function(..., comment = "#") {
  readr::read_tsv(..., comment = comment) %>%
    dplyr::rename(area = X1) %>%
    dplyr::rename_with(~ gsub("-", "_", .x, fixed = TRUE))
}


# Read data ---------------------------------------------------------------

translations <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc")

car_density <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "car_density_areas.txt"),
  col_types = "cd"
)
car_use <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "car_use_areas.txt"),
  col_types = "cd"
)
origin_demand <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "origin_demand_areas.txt"),
  col_types = "cdddd"
)
own_zone_demand <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "own_zone_demand.txt"),
  col_types = "cdddddddddddddddddddddddddddddddddddd"
)
vehicle_kms_modes <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "vehicle_kms_areas.txt"),
  col_types = "cddddddddd"
)
vehicle_kms_vdfs <- read_tsv_helmet(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "vehicle_kms_vdfs_areas.txt"),
  col_types = "cddddd"
)
# workforce_accessibility <- read_tsv_helmet(
#   file.path(config::get("helmet_data"),
#             config::get("results"),
#             "workforce_accessibility_per_area.txt"),
#   col_types = "cd"
# )

noise <- read_tsv(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "noise_areas.txt"),
  skip = 1, # skip old column names
  col_types = "cdd",
  col_names = c("area", "area_km2", "population") # override column names
)
aggregated_demand <- read_tsv(
  file.path(config::get("helmet_data"),
            config::get("results"),
            "aggregated_demand.txt"),
  col_types = "ccccd",
  col_names = c("area_origin", "area_destination", "purpose", "mode", "demand"),
  na = "nan"
)


# Read and aggregate zone data --------------------------------------------

zones <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario")))) %>%
  sf::st_drop_geometry()

zones1 <- zones %>%
  dplyr::group_by(area) %>%
  dplyr::summarise(
    total_pop = sum(total_pop),
    total_wrk = sum(total_wrk)
  )

zones2 <- zones %>%
  dplyr::group_by(area, savu_goodness, .drop = FALSE) %>%
  dplyr::summarise(
    goodness_wrk = sum(total_wrk)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(savu_goodness == "SAVU hyvä")


# Join data ---------------------------------------------------------------

# Rename columns to avoid name collisions
vehicle_kms_modes <- vehicle_kms_modes %>%
  dplyr::rename_with(~ paste0("vehicle_kms_", .x), -area)

areas <- data.frame(area = unique(zones$area)) %>%
  dplyr::left_join(zones1, by = "area") %>%
  dplyr::left_join(zones2, by = "area") %>%
  dplyr::left_join(vehicle_kms_modes, by = "area") %>%
  dplyr::left_join(car_density, by = "area")


# Impact assessment columns  ----------------------------------------------

areas <- areas %>%
  dplyr::mutate(vehicle_kms_total = rowSums(select(., starts_with("vehicle_kms")))) %>%
  dplyr::mutate(car_density = 1000 * car_density) %>%
  dplyr::mutate(goodness_share = goodness_wrk / total_wrk)


# Add total row -----------------------------------------------------------

areas <- areas %>%
  dplyr::add_row(
    area = "helsinki_region",
    car_density = weighted.mean(.$car_density, .$total_pop),
    goodness_share = sum(.$goodness_wrk) / sum(.$total_wrk)
  )


# Translate data ----------------------------------------------------------

areas <- areas %>%
  dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label)) %>%
  dplyr::arrange(area)


# Output ------------------------------------------------------------------

readr::write_rds(areas, file = here::here("results", sprintf("areas_%s.rds", config::get("scenario"))))
