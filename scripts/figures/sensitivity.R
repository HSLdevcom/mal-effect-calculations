# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

# TODO: These need to be looped through main scenarios.
areas_all <- dplyr::bind_rows(
  "2018 Nykytila" = readr::read_rds(here::here("results", "areas_2018.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "areas_2040_ve0.rds")),
  .id = "scenario")

# TODO: These need to be looped through sensitivity scenarios of every main
# scenario except the baseline scenario.
areas_sensitivity <- dplyr::bind_rows(
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "areas_2040_ve0.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "areas_2040_ve0_car--.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "areas_2040_ve0_car++.rds")),
  .id = "scenario")


# Sensitivity analysis ----------------------------------------------------

areas_sensitivity <- areas_sensitivity %>%
  dplyr::group_by(scenario, area) %>%
  dplyr::summarise(across(where(is.numeric), list(lower = min, upper = max), .names = "{.col}_{.fn}"), .groups = "drop")

areas_all <- areas_all %>%
  dplyr::left_join(areas_sensitivity, by = c("scenario", "area")) %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))


# Output ------------------------------------------------------------------

readr::write_rds(areas_all, file = here::here("results", "areas_all.rds"))
