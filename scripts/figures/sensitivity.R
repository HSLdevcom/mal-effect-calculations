# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

add_lower_and_upper_limits <- function(scenario_list, sensitivity_scenario_list, file_name, grouping_variables) {
  all <- read_and_bind(scenario_list, file_name)
  sensitivity <- read_and_bind(sensitivity_scenario_list, file_name)

  sensitivity <- sensitivity %>%
    # Values in main scenarios are always inside error bars
    dplyr::bind_rows(all) %>%
    dplyr::filter(scenario != sprintf("%i %s", scenarios$year[scenarios$present], scenarios$name[scenarios$present])) %>%
    dplyr::group_by(across(all_of(grouping_variables))) %>%
    dplyr::summarise(across(where(is.numeric), list(lower = min, upper = max),
                            .names = "{.col}_{.fn}"), .groups = "drop")

  all <- all %>%
    dplyr::left_join(sensitivity, by = grouping_variables) %>%
    dplyr::mutate(scenario = forcats::as_factor(scenario))
  return(all)
}

add_lower_and_upper_limits(scenario_list, sensitivity_scenario_list, "areas", c("scenario", "area")) %>%
  readr::write_rds(here::here("results", "areas_all.rds"))

add_lower_and_upper_limits(scenario_list, sensitivity_scenario_list, "emissions", c("scenario", "vehicle")) %>%
  readr::write_rds(here::here("results", "emissions_all.rds"))

add_lower_and_upper_limits(scenario_list, sensitivity_scenario_list, "vdfs", c("scenario", "vdf")) %>%
  readr::write_rds(here::here("results", "vdfs_all.rds"))

add_lower_and_upper_limits(scenario_list, sensitivity_scenario_list, "cargo", c("scenario", "group")) %>%
  readr::write_rds(here::here("results", "cargo_all.rds"))
