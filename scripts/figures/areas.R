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
  here::here("data", "Tulokset", "2020", "car_density_areas.txt"),
  col_types = "cd"
)
car_use <- read_tsv_helmet(
  here::here("data", "Tulokset", "2020", "car_use_areas.txt"),
  col_types = "cd"
)
origin_demand <- read_tsv_helmet(
  here::here("data", "Tulokset", "2020", "origin_demand_areas.txt"),
  col_types = "cdddd"
)
own_zone_demand <- read_tsv_helmet(
  here::here("data", "Tulokset", "2020", "own_zone_demand.txt"),
  col_types = "cdddddddddddddddddddddddddddddddddddd"
)
vehicle_kms_modes <- read_tsv_helmet(
  here::here("data", "Tulokset", "2020", "vehicle_kms_areas.txt"),
  col_types = "cddddddddd"
)
vehicle_kms_vdfs <- read_tsv_helmet(
  here::here("data", "Tulokset", "2020", "vehicle_kms_vdfs_areas.txt"),
  col_types = "cddddd"
)
workforce_accessibility <- read_tsv_helmet(
  here::here("data", "Tulokset", "2020", "workforce_accessibility_per_area.txt"),
  col_types = "cd"
)

noise <- read_tsv(
  here::here("data", "Tulokset", "2020", "noise_areas.txt"),
  skip = 1,
  col_types = "cdd",
  col_names = c("area", "area_km2", "population") # override column names
)
aggregated_demand <- read_tsv(
  here::here("data", "Tulokset", "2020", "aggregated_demand.txt"),
  col_types = "ccccd",
  col_names = c("area_origin", "area_destination", "purpose", "mode", "demand"),
  na = "nan"
)


# Join data ---------------------------------------------------------------

# Rename columns to avoid name collisions
vehicle_kms_modes <- vehicle_kms_modes %>%
  dplyr::rename_with(~ paste0("vehicle_kms_", .x), -area)

areas <- data.frame(area = c("helsinki_cbd",
                             "helsinki_other",
                             "espoo_vant_kau",
                             "surround_train",
                             "surround_other",
                             "peripheral")) %>%
  dplyr::left_join(vehicle_kms_modes, by = "area") %>%
  dplyr::left_join(car_density, by = "area")


# Translate data ----------------------------------------------------------

areas <- areas %>%
  dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label))


# Impact assessment columns  ----------------------------------------------

areas <- areas %>%
  dplyr::mutate(vehicle_kms_total = rowSums(select(., starts_with("vehicle_kms"))))


# Output ------------------------------------------------------------------

readr::write_rds(areas, file = here::here("results", "areas_2018.rds"))
