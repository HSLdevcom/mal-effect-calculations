# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

metro <- here::here("data", "RautatieViiva_Maastokartta100k_MML_Metro_HS15.gpkg") %>%
  sf::read_sf()

metro %>%
  readr::write_rds(here::here("results", "metro.rds"))
