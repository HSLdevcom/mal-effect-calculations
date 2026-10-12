# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

options(warn=1, scipen = 1000000)
Sys.setenv(R_CONFIG_ACTIVE = "default")


# Validate scenarios ------------------------------------------------------

source(here::here("scripts", "figures", "validate_scenarios.R"), encoding = "utf-8")


# Utility functions -------------------------------------------------------

source(here::here("scripts", "utils.R"), encoding = "utf-8")
source(here::here("scripts", "basemap", "run.R"), encoding = "utf-8")
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "basemap", "ensi.R"), encoding = "utf-8")
source(here::here("scripts", "basemap", "centers-and-stations.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "squares.R"), encoding = "utf-8")


# Plot common figures -----------------------------------------------------

source(here::here("scripts", "figures", "map_centers.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_centers_uusimaa-2050.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "map_hubs.R"), encoding = "utf-8")


# Prepare data ------------------------------------------------------------

# scenario_list <- c("2023", "2045_ve0")
# scenario_list <- c("2045_suunnitelma")
scenario_list <- c("2023", "2045_ve0", "2045_suunnitelma")

scenario <- "2023"
scenario_attributes <- set_scenario(scenario)
# message(sprintf("Prepare data in scenario %s...", scenario))
# source(here::here("scripts", "figures", "zones.R"), encoding = "utf-8")
# source(here::here("scripts", "figures", "links.R"), encoding = "utf-8")
# source(here::here("scripts", "figures", "areas.R"), encoding = "utf-8")
# source(here::here("scripts", "figures", "vdfs.R"), encoding = "utf-8")
#source(here::here("scripts", "figures", "emissions.R"), encoding = "utf-8")
# source(here::here("scripts", "figures", "cargo.R"), encoding = "utf-8")
# source(here::here("scripts", "figures", "centers.R"), encoding = "utf-8")

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


# Sensitivity analysis ----------------------------------------------------

sensitivity_scenario_list <- scenarios$scenario[scenarios$sensitivity]
sensitivity_scenario_list <- c()

for (scenario in sensitivity_scenario_list) {
  scenario_attributes <- set_scenario(scenario)
  message(sprintf("Prepare data in scenario %s...", scenario))
  source(here::here("scripts", "figures", "zones.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "links.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "areas.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "vdfs.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "emissions.R"), encoding = "utf-8")
  source(here::here("scripts", "figures", "cargo.R"), encoding = "utf-8")
}

#This generates _all.rds files
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

#TODO: Remove when plan scenarios are available
skipped = c("apartments", "twocenter")
for (skip in skipped) {
  singles <- singles[!grepl(skip, singles)]
}

#TODO: Remove when plan scenarios are available
skipped = c("ttime","malpakka","cba","speed","co2_time","delay", "twocenter")
for (skip in skipped) {
  differences <- differences[!grepl(skip, differences)]
}

#TODO: Remove when plan scenarios are available
skipped = c("ttime","malpakka","cba","speed","co2_time","delay", "twocenter")
for (skip in skipped) {
  multiples <- multiples[!grepl(skip, multiples)]
}


for (scenario in scenario_list) {
  scenario_attributes <- set_scenario(scenario)
  lapply(singles, verbose_source, encoding = "utf-8")
}

invisible(lapply(differences, verbose_source, encoding = "utf-8"))
invisible(lapply(multiples, verbose_source, encoding = "utf-8"))
source(here::here("scripts", "figures", "multiple","graph_mode-share_sustainable_cap_reg.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "multiple","graph_co2_2.R"), encoding = "utf-8")
source(here::here("scripts", "figures", "single","map_relative-speed.R"), encoding = "utf-8")
