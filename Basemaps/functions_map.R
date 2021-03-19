# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

region <- readr::read_rds(here::here("Basemaps", "region.rds"))
bonus_region <- readr::read_rds(here::here("Basemaps", "bonus_region.rds"))
metro <- readr::read_rds(here::here("Basemaps", "metro.rds"))
train <- readr::read_rds(here::here("Basemaps", "train.rds"))
roads <- readr::read_rds(here::here("Basemaps", "roads.rds"))
municipalities <- readr::read_rds(here::here("Basemaps", "municipalities.rds"))
water <- readr::read_rds(here::here("Basemaps", "water.rds"))

bbox <- sf::st_bbox(region)

geom_basemap <- function() {
  list(
    geom_sf(data = water, fill = "#a6cee3", color = NA, size = 0),
    geom_sf(data = municipalities, color = "#9b3b6b", fill = NA, size = 0.26),
    geom_sf(data = roads, color = "#64a5a5", size = 0.2),
    geom_sf(data = train, color = "#000000", linetype = "solid", size = 0.66),
    geom_sf(data = train, color = "#ffffff", linetype = "22", size = 0.53),
    geom_sf(data = metro, color = "#ff7f00", linetype = "solid", size = 0.66),
    geom_sf(data = metro, color = "#ffffff", linetype = "22", size = 0.53),
    geom_sf(data = region, color = "#000000", fill = NA, size = 0.46),
    geom_sf(data = bonus_region, color = "#000000", fill = NA, linetype = "33", size = 0.46)
  )
}

