# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

region <- here::here("Basemaps", "municipalities.rds") %>%
  readr::read_rds() %>%
  sf::st_union()

region %>%
  readr::write_rds(here::here("Basemaps", "region.rds"))
