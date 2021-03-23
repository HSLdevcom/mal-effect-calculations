# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

water <- here::here("Basemaps", "input", "meri.shp") %>%
  sf::read_sf(options = "ENCODING=WINDOWS-1252") %>%
  sf::st_set_crs(3879)

water %>%
  readr::write_rds(here::here("Basemaps", "water.rds"))
