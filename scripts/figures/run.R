# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

Sys.setenv(R_CONFIG_ACTIVE = "default")


# Validate scenarios ------------------------------------------------------

source(here::here("scripts", "figures", "validate_scenarios.R"), encoding = "utf-8")


# Utility functions -------------------------------------------------------

source(here::here("scripts", "basemap", "run.R"), encoding = "utf-8")
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


# Plot common figures -----------------------------------------------------

source(here::here("scripts", "figures", "map_centers.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_centers_uusimaa-2050.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_hubs.R"), encoding = "utf-8")


# Prepare data ------------------------------------------------------------

scenario_list <- c("2018", "2040_ve0", "2040_ve1")

for (scenario in scenario_list) {
  scenario_attributes <- set_scenario(scenario)
  message(sprintf("Prepare data in scenario %s...", scenario))
  source(here::here("scripts", "figures", "zones.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "links.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "areas.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "vdfs.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "emissions.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "cargo.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "centers.R"), encoding = "utf-8")
}

read_and_bind(scenario_list, "areas") %>%
  readr::write_rds(here::here("results", "areas_all.rds"))

read_and_bind(scenario_list, "vdfs") %>%
  readr::write_rds(here::here("results", "vdfs_all.rds"))

read_and_bind(scenario_list, "emissions") %>%
  readr::write_rds(here::here("results", "emissions_all.rds"))

read_and_bind(scenario_list, "cargo") %>%
  readr::write_rds(here::here("results", "cargo_all.rds"))


# Sensitivity analysis ----------------------------------------------------

sensitivity_scenarios <- c("2040_ve0_etatyo", "2040_ve0_jklmaksi", "2040_ve0_mini", "2040_ve0_muulitar", "2040_ve0_tieruuhka")

for (scenario in sensitivity_scenarios) {
  Sys.setenv(R_CONFIG_ACTIVE = scenario)
  message(sprintf("Prepare data in scenario %s...", scenario))
  source(here::here("scripts", "figures", "zones.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "areas.R"), encoding = "utf-8")
}

source(here::here("scripts", "figures", "sensitivity.R"), encoding = "utf-8")


# Plot figures -----------------------------------------------------------

singles <- list.files(here::here("scripts", "figures", "single"),
                      pattern = ".R$",
                      full.names = TRUE)
differences <- list.files(here::here("scripts", "figures", "difference"),
                      pattern = ".R$",
                      full.names = TRUE)
multiples <- list.files(here::here("scripts", "figures", "multiple"),
                      pattern = ".R$",
                      full.names = TRUE)

for (scenario in scenario_list) {
  scenario_attributes <- set_scenario(scenario)
  lapply(singles, verbose_source, encoding = "utf-8")
}

invisible(lapply(differences, verbose_source, encoding = "utf-8"))
invisible(lapply(multiples, verbose_source, encoding = "utf-8"))
