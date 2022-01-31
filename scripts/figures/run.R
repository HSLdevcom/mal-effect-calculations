# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Utility functions -------------------------------------------------------

source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


# Plot common figures -----------------------------------------------------

source(here::here("scripts", "figures", "map_centers.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_centers_uusimaa-2050.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_hubs.R"), encoding = "utf-8")


# Prepare data ------------------------------------------------------------

scenarios <- c("2018", "2040_ve0")
scenario_titles <- NULL

for (scenario in scenarios) {
  Sys.setenv(R_CONFIG_ACTIVE = scenario)
  message(sprintf("Prepare data in scenario %s...", scenario))
  scenario_titles <- c(scenario_titles,
                       sprintf("%d %s", config::get("year"), config::get("scenario_name")))
  source(here::here("scripts", "figures", "zones.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "links.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "areas.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "vdfs.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "emissions.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "centers.R"), encoding = "utf-8")
}

Sys.setenv(R_CONFIG_ACTIVE = "default")

areas_all <- dplyr::bind_rows(
  "2018 Nykytila" = readr::read_rds(here::here("results", "areas_2018.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "areas_2040_ve0.rds")),
  .id = "scenario") %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))

readr::write_rds(areas_all, file = here::here("results", "areas_all.rds"))

vdfs_all <- dplyr::bind_rows(
  "2018 Nykytila" = readr::read_rds(here::here("results", "vdfs_2018.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "vdfs_2040_ve0.rds")),
  .id = "scenario") %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))

readr::write_rds(vdfs_all, file = here::here("results", "vdfs_all.rds"))

emissions_all <- dplyr::bind_rows(
  "2018 Nykytila" = readr::read_rds(here::here("results", "emissions_2018.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "emissions_2040_ve0.rds")),
  .id = "scenario") %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))

readr::write_rds(emissions_all, file = here::here("results", "emissions_all.rds"))


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

for (scenario in scenarios) {
  Sys.setenv(R_CONFIG_ACTIVE = scenario)
  lapply(singles, verbose_source, encoding = "utf-8")
}

Sys.setenv(R_CONFIG_ACTIVE = "default")
invisible(lapply(differences, verbose_source, encoding = "utf-8"))
invisible(lapply(multiples, verbose_source, encoding = "utf-8"))
