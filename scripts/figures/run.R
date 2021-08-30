# -*- coding: utf-8-unix -*-
library(here)

source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

verbose_source <- function(file, ...) {
  message(sprintf("Running analysis in %s...", basename(file)))
  invisible(source(file, ...))
}

# When running maps and graphs, the order of the files does not matter.
files_in <- list.files(here::here("scripts", "figures"),
                       pattern = "^map.*R$",
                       full.names = TRUE)

for (scenario in c("2020", "2040_ve0", "2040_ve0_muulitar")) {
  Sys.setenv(R_CONFIG_ACTIVE = scenario)
  source(here::here("scripts", "figures", "zones.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "areas.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "vdfs.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "graph_centers.R"), encoding = "utf-8")
  lapply(files_in, verbose_source, encoding = "utf-8")
}

Sys.setenv(R_CONFIG_ACTIVE = "default")

areas_all <- dplyr::bind_rows(
  "2020 Nykytila" = readr::read_rds(here::here("results", "areas_2020.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "areas_2040_ve0.rds")),
  "2040 Etätyö+" = readr::read_rds(here::here("results", "areas_2040_ve0_muulitar.rds")),
  .id = "scenario") %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))

readr::write_rds(areas_all, file = here::here("results", "areas_all.rds"))

vdfs_all <- dplyr::bind_rows(
  "2020 Nykytila" = readr::read_rds(here::here("results", "vdfs_2020.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "vdfs_2040_ve0.rds")),
  "2040 Etätyö+" = readr::read_rds(here::here("results", "vdfs_2040_ve0_muulitar.rds")),
  .id = "scenario") %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))

readr::write_rds(vdfs_all, file = here::here("results", "vdfs_all.rds"))

# When running maps and graphs, the order of the files does not matter.
# files_in <- list.files(here::here("scripts", "figures"),
#                        pattern = "^graph.*R$",
#                        full.names = TRUE)
# lapply(files_in, verbose_source, encoding = "utf-8")
