# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

train <- here::here("data", "RautatieViiva_Maastokartta100k_MML_Henkilorata_HS15.gpkg") %>%
  sf::read_sf()

train %>%
  readr::write_rds(here::here("results", "train.rds"))
