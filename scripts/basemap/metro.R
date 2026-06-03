# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

metro <- here::here("data", "metro2023.gpkg") %>%
  sf::read_sf()

metro %>%
  readr::write_rds(here::here("results", "metro.rds"))
