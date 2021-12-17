# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

metro <- here::here("data", "lansimetron_jatke.shp") %>%
  sf::read_sf()

metro %>%
  readr::write_rds(here::here("results", "metro_2040_ve0.rds"))
