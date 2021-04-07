# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

metro1 <- here::here("data", "Länsimetron_jatke.TAB") %>%
  sf::read_sf() %>%
  sf::st_set_crs(3879)

metro2 <- here::here("data", "Lansimetro.tab") %>%
  sf::read_sf() %>%
  sf::st_set_crs(3879)

metro3 <- here::here("data", "metro.shp") %>%
  sf::read_sf() %>%
  sf::st_set_crs(3879)

metro <- dplyr::bind_rows(metro1, metro2, metro3) %>%
  sf::st_union()

metro %>%
  readr::write_rds(here::here("results", "metro.rds"))
