# -*- coding: utf-8-unix -*-
# To rerun figures in a fresh session, source this script and then run the
# "Plot figures" section of run.R. No preparation scripts need to be run.
# The RDS files must come from a completed run using the current scenarios.tsv.
library(here)
library(tidyverse)
library(sf)

options(warn = 1, scipen = 1000000, readr.show_progress = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "default")

source(here::here("scripts", "figures", "validate_scenarios.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")

scenario_list <- scenarios$scenario[!scenarios$sensitivity]
sensitivity_scenario_list <- scenarios$scenario[scenarios$sensitivity]

# Use the projected scenario for individually sourced plots. The plotting loop
# in run.R sets scenario_attributes separately for each main scenario.
scenario <- scenarios$scenario[scenarios$projected][1]
scenario_attributes <- set_scenario(scenario)

local({
  # Keep cached objects in their original structures, including sf geometries,
  # factor levels, and the sensitivity bounds already present in *_all.rds.
  shared <- c(
    region = "region", bonus_region = "bonus_region", metro = "metro",
    train = "train", roads = "roads", municipalities = "municipalities",
    water = "water", squares = "squares", ensi = "ensi",
    ensi_ruudut = "ensi_ruudut", centers_and_stations = "centers-and-stations",
    centers_uusimaa_2050 = "centers_uusimaa-2050",
    sijoittelualueet2023 = "sijoittelualueet2023",
    squares_areas_2045_ve0 = "squares_areas_2045_ve0",
    squares_areas_2045_tavoiteverkko = "squares_areas_2045_tavoiteverkko",
    squares_areas_2045_suunnitelma = "squares_areas_2045_suunnitelma",
    areas_all = "areas_all", emissions_all = "emissions_all",
    vdfs_all = "vdfs_all", cargo_all = "cargo_all"
  )
  scenario_prefixes <- c(
    zones = "zones", links = "links", buffers = "buffers",
    buffers_vrk = "buffers-vrk", buffers_car = "buffers-car",
    buffers_car_vrk = "buffers-car-vrk", buffers_truck_all = "buffers-truck_all",
    buffers_truck_all_vrk = "buffers-truck_all-vrk", areas = "areas",
    emissions = "emissions", vdfs = "vdfs", cargo = "cargo", ttimes = "centers"
  )
  cached_objects <- c(shared, stats::setNames(
    paste0(scenario_prefixes, "_", scenario), names(scenario_prefixes)
  ))

  # Most single/difference/multiple plots read these files themselves. Check
  # every main scenario without retaining all their spatial data in memory.
  required <- unique(c(
    unname(shared), "zones",
    as.vector(outer(unname(scenario_prefixes), scenario_list, paste, sep = "_"))
  ))
  required_paths <- here::here("results", paste0(required, ".rds"))
  missing <- required_paths[!file.exists(required_paths)]
  if (length(missing) > 0) {
    stop(
      "Missing figure cache files:\n", paste(missing, collapse = "\n"),
      "\nRun the corresponding preparation steps in scripts/figures/run.R first.",
      call. = FALSE
    )
  }

  objects <- lapply(cached_objects, function(filename) {
    readr::read_rds(here::here("results", paste0(filename, ".rds")))
  })

  # These belong to the separate health-centre workflow and may not exist yet.
  optional <- c(
    health_centers = "health-centers",
    missing_coordinates = "health-centers-missing-coordinates",
    service_areas = "health-centre-service-areas"
  )
  for (object in names(optional)) {
    path <- here::here("results", paste0(optional[[object]], ".rds"))
    if (file.exists(path)) objects[[object]] <- readr::read_rds(path)
  }
  list2env(objects, envir = parent.env(environment()))
})

# Defines palettes, map/graph helpers and bbox using the cached basemap layers.
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

# Later specialist scripts still read their own inputs: health-centre travel
# times and transfers use OMX matrices; sustainable_accessibility.R uses text,
# population and geometry inputs; transit crowding uses links_toolbox_*.gpkg.
message("Figure cache loaded from results; ready for the Plot figures section of run.R.")
