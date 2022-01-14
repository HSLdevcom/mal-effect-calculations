# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Read data ---------------------------------------------------------------

region <- readr::read_rds(here::here("results", "region.rds"))

links <- here::here(config::get("helmet_data"), config::get("results"), "links.txt") %>%
  readr::read_tsv() %>%
  dplyr::rename_with(~ gsub("@", "", .x, fixed = TRUE)) %>%
  dplyr::rename(geometry = Link) %>%
  sf::st_as_sf(wkt = "geometry", remove = TRUE, crs = 3879) %>%
  dplyr::filter(!(volume_delay_func %in% 99)) %>%
  dplyr::mutate(volume_aht = car_leisure_aht + car_work_aht + bus_aht + trailer_truck_aht + truck_aht + van_aht,
                car_aht = car_work_aht + car_leisure_aht,
                relative_speed = car_time_pt / car_time_aht) %>%
  # Filter for improved plotting
  dplyr::filter(sf::st_intersects(., sf::st_as_sf(region), sparse = FALSE))

buffers <- links %>%
  sf::st_buffer(dist = -links$volume_aht / 5,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(volume_aht > 0.01)

buffers_car <- links %>%
  sf::st_buffer(dist = -links$car_aht / 5,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(car_aht > 0.01)


# Output ------------------------------------------------------------------

readr::write_rds(links, file = here::here("results", sprintf("links_%s.rds", config::get("scenario"))))
readr::write_rds(buffers, file = here::here("results", sprintf("buffers_%s.rds", config::get("scenario"))))
readr::write_rds(buffers_car, file = here::here("results", sprintf("buffers-car_%s.rds", config::get("scenario"))))
