# -*- coding: utf-8-unix -*-
library(here)

source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

verbose_source <- function(file, ...) {
  message(sprintf("Running analysis in %s...", basename(file)))
  invisible(source(file, ...))
}

source(here::here("scripts", "figures", "map_centers.R"), encoding = "utf-8")

for (scenario in c("2018", "2040_ve0", "2040_ve0_muulitar")) {
  Sys.setenv(R_CONFIG_ACTIVE = scenario)
  source(here::here("scripts", "figures", "zones.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "areas.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "vdfs.R"), encoding = "utf-8")

  source(here::here("scripts", "figures", "graph_centers.R"), encoding = "utf-8")

  source(here::here("scripts", "figures", "map_car-density.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "map_mode-share_bike-walk.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "map_mode-share_sustainable.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "map_savu.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "map_savu_workplaces.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "map_workforce-accessibility.R"), encoding = "utf-8")
}

Sys.setenv(R_CONFIG_ACTIVE = "default")

areas_all <- dplyr::bind_rows(
  "2018 Nykytila" = readr::read_rds(here::here("results", "areas_2018.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "areas_2040_ve0.rds")),
  "2040 Etätyö+" = readr::read_rds(here::here("results", "areas_2040_ve0_muulitar.rds")),
  .id = "scenario") %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))

readr::write_rds(areas_all, file = here::here("results", "areas_all.rds"))

vdfs_all <- dplyr::bind_rows(
  "2018 Nykytila" = readr::read_rds(here::here("results", "vdfs_2018.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "vdfs_2040_ve0.rds")),
  "2040 Etätyö+" = readr::read_rds(here::here("results", "vdfs_2040_ve0_muulitar.rds")),
  .id = "scenario") %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))

readr::write_rds(vdfs_all, file = here::here("results", "vdfs_all.rds"))

# When running maps and graphs, the order of the files does not matter.
source(here::here("scripts", "figures", "graph_car_density.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "graph_co2.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "graph_mode-share_sustainable.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "graph_pop.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "graph_savu_workplaces.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "graph_vehicle-kms-vdf.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "graph_vehicle_kms.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "graph_wrk.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "graph_workforce-accessibility.R"), encoding = "utf-8")

source(here::here("scripts", "figures", "map_diff_car-density.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_diff_mode-share_sustainable.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_diff_workforce-accessibility.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_diff_rel_workforce-accessibility.R"), encoding = "utf-8")
