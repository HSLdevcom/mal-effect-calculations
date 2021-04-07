# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

roads <- here::here("data", "päätieverkko_tomtom.shp") %>%
  sf::read_sf() %>%
  sf::st_transform(3879) %>%
  sf::st_union()

roads %>%
  readr::write_rds(here::here("results", "roads.rds"))
