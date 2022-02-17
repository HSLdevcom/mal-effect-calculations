# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

stations <- here::here("data", "RautatiePiste_Maastokartta100k_MML_HS15.gpkg") %>%
  sf::read_sf() %>%
  dplyr::rename(geometry = geom)

metro_stations_2040_ve0 <- here::here("data", "Lansimetron_jatkeen_asemat.shp") %>%
  sf::read_sf()

stations <- dplyr::bind_rows(stations, metro_stations_2040_ve0)

stations %>%
  readr::write_rds(here::here("results", "stations.rds"))
