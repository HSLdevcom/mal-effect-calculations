# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

train1 <- here::here("Basemaps", "input", "keharata.tab") %>%
  sf::read_sf(options = "ENCODING=WINDOWS-1252") %>%
  sf::st_set_crs(3879)

train2 <- here::here("Basemaps", "input", "juna_leik.shp") %>%
  sf::read_sf(options = "ENCODING=WINDOWS-1252") %>%
  sf::st_set_crs(3879)

train <- dplyr::bind_rows(train1, train2) %>%
  sf::st_union()

train %>%
  readr::write_rds(here::here("Basemaps", "train.rds"))
