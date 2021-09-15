# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

roads <- here::here("data", "TieViiva_Maastokartta100k_MML_Paatiet_HS15.gpkg") %>%
  sf::read_sf()

roads %>%
  readr::write_rds(here::here("results", "roads.rds"))
