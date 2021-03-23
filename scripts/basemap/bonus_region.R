# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

bonus_region <- here::here("Basemaps", "municipalities.rds") %>%
  readr::read_rds() %>%
  dplyr::filter(namefin %in% "Siuntio")

bonus_region %>%
  readr::write_rds(here::here("Basemaps", "bonus_region.rds"))
