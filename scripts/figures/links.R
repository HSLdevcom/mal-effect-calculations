# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(lwgeom)


# Read data ---------------------------------------------------------------

region <- readr::read_rds(here::here("results", "region.rds"))
zones <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))

links <- here::here(config::get("helmet_data"), config::get("results"), "links.txt") %>%
  readr::read_tsv() %>%
  dplyr::rename_with(~ gsub("@", "", .x, fixed = TRUE)) %>%
  dplyr::rename(geometry = Link) %>%
  sf::st_as_sf(wkt = "geometry", remove = TRUE, crs = 3879) %>%
  dplyr::filter(!(volume_delay_func %in% 99)) %>%
  # Remove links where only walking and bicycling are allowed
  dplyr::filter(!(type %in% 70)) %>%
  # Remove rail links
  dplyr::filter(!type %in% 2:6) %>%
  dplyr::mutate(volume_aht = car_leisure_aht + car_work_aht + bus_aht + trailer_truck_aht + truck_aht + van_aht,
                car_aht = car_work_aht + car_leisure_aht,
                relative_speed = car_time_pt / car_time_aht) %>%
  dplyr::mutate(
    length = units::drop_units(sf::st_length(.)),
    free_speed = pmax(data2, 0),
    free_time = (length / 1000) / free_speed,
    diff_car_time_aht = pmax((car_time_aht / 60) - free_time, 0),
    diff_car_time_iht = pmax((car_time_iht / 60) - free_time, 0),
    diff_car_time_pt = pmax((car_time_pt / 60) - free_time, 0),
    truck_all_aht = 5 * (trailer_truck_aht + truck_aht) * diff_car_time_aht,
    truck_all_iht = 5 * (trailer_truck_iht + truck_iht) * diff_car_time_iht,
    truck_all_pt = 5 * (trailer_truck_pt + truck_pt) * diff_car_time_pt,
    # TODO: Remove bus lines
    transit_aht = (transit_work_aht + transit_leisure_aht) * diff_car_time_aht,
    transit_iht = (transit_work_iht + transit_leisure_iht) * diff_car_time_iht,
    transit_pt = (transit_work_pt + transit_leisure_pt) * diff_car_time_pt,
    car_aht = (car_work_aht + car_leisure_aht + van_aht) * diff_car_time_aht,
    car_iht = (car_work_iht + car_leisure_iht + van_iht) * diff_car_time_iht,
    car_pt = (car_work_pt + car_leisure_pt + van_pt) * diff_car_time_pt
  ) %>%
  # Filter for improved plotting
  dplyr::filter(sf::st_intersects(., sf::st_as_sf(region), sparse = FALSE))

areas <- links %>%
  dplyr::select(data1) %>%
  # Get starting points of each linestring
  sf::st_set_geometry(lwgeom::st_startpoint(.)) %>%
  # Find out which area each starting point belongs to. Using
  # `sf::st_nearest_feature` because links at the region border are not all
  # matched with `sf::st_intersects`.
  sf::st_join(zones, join = sf::st_nearest_feature) %>%
  dplyr::select(area) %>%
  sf::st_drop_geometry()

stopifnot(nrow(areas) == nrow(links))
links$area <- areas$area

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
