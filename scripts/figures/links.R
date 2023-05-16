# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(lwgeom)


# Read data ---------------------------------------------------------------

region <- readr::read_rds(here::here("results", "region.rds"))
volume_factors <- readr::read_tsv(here::here("utilities", "volume_factors.tsv"), col_types = "cddd")
zones <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario_attributes[["scenario"]])))

links <- here::here(config::get("helmet_data"), scenario_attributes[["results"]], "links.txt") %>%
  readr::read_tsv(col_types = cols(Link = col_character(),
                                   .default = col_double())) %>%
  dplyr::rename_with(~ gsub("@", "", .x, fixed = TRUE)) %>%
  dplyr::rename(geometry = Link) %>%
  sf::st_as_sf(wkt = "geometry", remove = TRUE, crs = 3879) %>%
  # Add unique column for identification
  dplyr::mutate(fid = dplyr::row_number())

areas <- links %>%
  dplyr::select(data1) %>%
  # Get starting points of each linestring
  sf::st_set_geometry(lwgeom::st_startpoint(.)) %>%
  # Find out which area each starting point belongs to. Using
  # `sf::st_nearest_feature` because links outside region are not all
  # matched with `sf::st_intersects`.
  sf::st_join(zones, join = sf::st_nearest_feature) %>%
  dplyr::select(area) %>%
  # Links outside region have area = "peripheral"
  dplyr::mutate(area = dplyr::if_else(matrix_col_to_vector(sf::st_intersects(., sf::st_as_sf(region), sparse = FALSE)), area, "peripheral")) %>%
  sf::st_drop_geometry()

stopifnot(nrow(areas) == nrow(links))
links$area <- areas$area

links %>% sf::write_sf(here::here("results", sprintf("links_toolbox_%s.gpkg", scenario_attributes[["scenario"]])), append = FALSE, delete_dsn = TRUE)

links <- links %>%
  dplyr::filter(!(volume_delay_func %in% 99)) %>%
  # Remove links where only walking and bicycling are allowed
  dplyr::filter(!(type %in% 70)) %>%
  # Remove rail links
  dplyr::filter(!type %in% 2:6) %>%
  dplyr::mutate(volume_vrk = car_leisure_vrk + car_work_vrk + bus_vrk + trailer_truck_vrk + truck_vrk + van_vrk,
                car_vrk = car_work_vrk + car_leisure_vrk,
                truck_all_vrk = trailer_truck_vrk + truck_vrk,
                volume_aht = car_leisure_aht + car_work_aht + bus_aht + trailer_truck_aht + truck_aht + van_aht,
                car_aht = car_work_aht + car_leisure_aht,
                truck_all_aht = trailer_truck_aht + truck_aht,
                relative_speed = (60 * length / data2) / car_time_aht,
                # If car_time_aht is -1, the link is disabled for car modes, and
                # relative speed will be interpreted as NA
                relative_speed = dplyr::if_else(relative_speed < 0, NA_real_, relative_speed)) %>%
  # Filter for improved plotting
  dplyr::filter(area != "peripheral")

volume_factors <- volume_factors %>%
  tidyr::pivot_longer(-mode, names_to = "period", values_to = "volume_factor")


links0 <- links %>%
  sf::st_drop_geometry() %>%
  dplyr::select(fid, data2, length, type,
                dplyr::starts_with(c("car_time", "car_work", "car_leisure",
                                     "van", "truck", "trailer_truck",
                                     "transit_work", "transit_leisure"))) %>%
  dplyr::select(!dplyr::ends_with("vrk")) %>%
  tidyr::pivot_longer(-c(fid, data2, length, type, dplyr::starts_with("car_time")),
                      names_to = c("mode", "period"),
                      names_pattern = "([a-z_]+)_([a-z]+)",
                      values_to = "volume") %>%
  dplyr::mutate(car_time = dplyr::case_when(
    period == "aht" ~ car_time_aht,
    period == "pt"  ~ car_time_pt,
    period == "iht" ~ car_time_iht,
    TRUE ~ NA_real_
  )) %>%
  dplyr::select(-c(car_time_aht, car_time_pt, car_time_iht)) %>%
  dplyr::mutate(
    free_speed = pmax(data2, 0), # km/h
    free_time = length / free_speed, # h
    diff_car_time = pmax((car_time / 60) - free_time, 0)) %>%  # h
  dplyr::left_join(volume_factors, by = c("mode", "period")) %>%
  dplyr::mutate(delay = volume / volume_factor * diff_car_time) %>%
  # It was discussed and decided that bus passengers do not experience delays
  # when they travel on a road with a separate line for buses. This is handled
  # by forcing the time difference to zero on those links.
  dplyr::mutate(
    delay = dplyr::case_when(
      mode %in% c("transit_work", "transit_leisure") & period == "aht" & type %in% c(200:499, 600:699) ~ 0.0,
      mode %in% c("transit_work", "transit_leisure") & period == "pt" & type %in% c(300:399, 600:699) ~ 0.0,
      mode %in% c("transit_work", "transit_leisure") & period == "iht" & type %in% c(200:399, 500:699) ~ 0.0,
      TRUE ~ delay
    )
  ) %>%
  dplyr::group_by(fid, mode) %>%
  dplyr::summarise(delay = sum(delay), .groups = "drop") %>%
  tidyr::pivot_wider(id_cols = fid, names_from = "mode", values_from = "delay") %>%
  dplyr::mutate(weighted_delay_car_all = car_leisure + car_work + van,
                weighted_delay_truck_all = 5 * (trailer_truck + truck),
                weighted_delay_transit =  transit_leisure + transit_work,
                weighted_delay_all = weighted_delay_car_all + weighted_delay_truck_all + weighted_delay_transit) %>%
  dplyr::select(fid, dplyr::starts_with("weighted_delay"))

links <- links %>%
  dplyr::left_join(links0, by = "fid")

buffers_vrk <- links %>%
  sf::st_buffer(dist = -links$volume_vrk / 100,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(volume_vrk > 0.01)

buffers_car_vrk <- links %>%
  sf::st_buffer(dist = -links$car_vrk / 100,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(car_vrk > 0.01)

buffers_truck_all_vrk <- links %>%
  sf::st_buffer(dist = -links$truck_all_vrk / 8,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(truck_all_vrk > 0.01)

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

buffers_truck_all <- links %>%
  sf::st_buffer(dist = -links$truck_all_aht * 3,
                endCapStyle = "FLAT",
                singleSide = TRUE) %>%
  # Filter for improved plotting
  dplyr::filter(truck_all_aht > 0.01)


# Output ------------------------------------------------------------------

readr::write_rds(links, file = here::here("results", sprintf("links_%s.rds", scenario_attributes[["scenario"]])))
readr::write_rds(buffers_vrk, file = here::here("results", sprintf("buffers-vrk_%s.rds", scenario_attributes[["scenario"]])))
readr::write_rds(buffers_car_vrk, file = here::here("results", sprintf("buffers-car-vrk_%s.rds", scenario_attributes[["scenario"]])))
readr::write_rds(buffers_truck_all_vrk, file = here::here("results", sprintf("buffers-truck_all-vrk_%s.rds", scenario_attributes[["scenario"]])))
readr::write_rds(buffers, file = here::here("results", sprintf("buffers_%s.rds", scenario_attributes[["scenario"]])))
readr::write_rds(buffers_car, file = here::here("results", sprintf("buffers-car_%s.rds", scenario_attributes[["scenario"]])))
readr::write_rds(buffers_truck_all, file = here::here("results", sprintf("buffers-truck_all_%s.rds", scenario_attributes[["scenario"]])))
