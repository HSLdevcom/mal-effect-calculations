# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

municipalities <- here::here("data", "SuomenKuntajako_2021_100k_hs15.gpkg") %>%
  sf::read_sf() %>%
  sf::st_transform(3879)

municipalities %>%
  readr::write_rds(here::here("results", "municipalities.rds"))
