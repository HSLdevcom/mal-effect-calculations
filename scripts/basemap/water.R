# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

water1 <- here::here("data", "mtkmaasto_vesi_hs15.gpkg") %>%
  sf::read_sf(layer = "meri")

water2 <- here::here("data", "mtkmaasto_vesi_hs15.gpkg") %>%
  sf::read_sf(layer = "jarvi")

water3 <- here::here("data", "mtkmaasto_vesi_hs15.gpkg") %>%
  sf::read_sf(layer = "virtavesialue")

water <- dplyr::bind_rows(water1, water2, water3) %>%
  sf::st_transform(3879)

water %>%
  readr::write_rds(here::here("results", "water.rds"))
