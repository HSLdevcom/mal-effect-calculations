# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

zone_names <- vector(mode = "list", length = 3)
names(zone_names) <- scenario_list

for (scenario in scenario_list) {
  scenario_attributes <- set_scenario(scenario)
  zones <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))
  zones <- zones %>%
    dplyr::select(!c(FID_1, WSP_SIJ, WSP_ENN, SIJ2016, SIJ_MAARA, SIJ_ID, ENN2016)) %>%
    dplyr::relocate(zone) %>%
    dplyr::relocate(area, .after = zone) %>%
    dplyr::relocate(KUNTANIMI, .after = area)
  zone_names[[scenario]] <- names(zones)
  zones %>% sf::write_sf(here::here("results", sprintf("zones_%s.gpkg", scenario_attributes[["scenario"]])))
}

all(zone_names[[1]] == zone_names[[2]])
zone_names[[3]][!(zone_names[[3]] %in% zone_names[[2]])]

