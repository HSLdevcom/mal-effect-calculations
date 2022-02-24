# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

areas_all <- read_and_bind(scenario_list, "areas")
areas_sensitivity <- read_and_bind(sensitivity_scenario_list, "areas")


# Sensitivity analysis ----------------------------------------------------

areas_sensitivity <- areas_sensitivity %>%
  dplyr::group_by(scenario, area) %>%
  dplyr::summarise(across(where(is.numeric), list(lower = min, upper = max),
                          .names = "{.col}_{.fn}"), .groups = "drop")

areas_all <- areas_all %>%
  dplyr::left_join(areas_sensitivity, by = c("scenario", "area")) %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))


# Output ------------------------------------------------------------------

readr::write_rds(areas_all, file = here::here("results", "areas_all.rds"))
