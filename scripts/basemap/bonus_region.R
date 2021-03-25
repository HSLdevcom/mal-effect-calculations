# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

bonus_region <- here::here("results", "municipalities.rds") %>%
  readr::read_rds() %>%
  dplyr::filter(namefin %in% "Siuntio")

bonus_region %>%
  readr::write_rds(here::here("results", "bonus_region.rds"))
