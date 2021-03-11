# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

municipalities <- here::here("Basemaps", "input", "kuntarajat.shp") %>%
  sf::read_sf(options = "ENCODING=WINDOWS-1252") %>%
  sf::st_set_crs(3879)

municipalities %>%
  readr::write_rds(here::here("Basemaps", "municipalities.rds"))
