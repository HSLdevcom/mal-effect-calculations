# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

zones <- readr::read_rds(here::here("results", "zones.rds"))

centers <- here::here("data", "Uusimaa-kaava-2050_Pisteet.gpkg") %>%
  sf::read_sf() %>%
  sf::st_transform(3879) %>%
  dplyr::filter(grepl("^Keskustatoimintojen alue", kuvaus)) %>%
  dplyr::filter(sf::st_intersects(., sf::st_combine(zones), sparse = FALSE)) %>%
  sf::st_join(zones, join = st_within)

centers %>%
  readr::write_rds(here::here("results", "centers_uusimaa-2050.rds"))
