# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

region <- here::here("Basemaps", "input", "helsingin_seutu_pohja.shp") %>%
  sf::read_sf() %>%
  sf::st_union() %>%
  sf::st_transform(3879)

region %>%
  readr::write_rds(here::here("Basemaps", "region.rds"))
