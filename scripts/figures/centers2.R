# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(omxr)


# Read data ---------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "2018")

zones <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))
centers <- readr::read_rds(here::here("results", "centers_uusimaa-2050.rds"))

ttimes_pt <- read_helmet_omx(file.path(config::get("helmet_data"),
                                       config::get("results"),
                                       "Matrices",
                                       "time_pt.omx")) %>%
  dplyr::filter(origin %in% zones$zone & destination %in% centers$SIJ2019) %>%
  dplyr::rename(car = car_work,
                transit = transit_work,
                bike = bike,
                walk = walk) %>%
  dplyr::select(origin, destination, car, transit, bike, walk)

ttimes <- ttimes_pt %>%
  tidyr::pivot_longer(
    cols = car:walk,
    values_to = "ttime",
    names_to = "mode"
  ) %>%
  dplyr::group_by(origin, mode) %>%
  dplyr::arrange(ttime) %>%
  dplyr::summarise(accessibility2 = sum(ttime[1:2]), .groups = "drop")

ttimes_range <- ttimes %>%
  dplyr::filter(!(origin %in% 1531)) %>%
  dplyr::group_by(mode) %>%
  dplyr::summarise(amin = min(accessibility2),
                   amax = max(accessibility2))

ttimes2 <- ttimes %>%
  dplyr::left_join(ttimes_range, by = "mode") %>%
  dplyr::mutate(
    accessibility2_normal =  (100 - 1) * (accessibility2 - amin) / (amax - amin) + 1
  ) %>%
  dplyr::mutate(
    accessibility2_normal = pmax(1, pmin(accessibility2_normal, 100))
  ) %>%
  tidyr::pivot_wider(
    id_cols = origin,
    names_from = mode,
    values_from = accessibility2_normal
  )


# Output ------------------------------------------------------------------

readr::write_rds(ttimes2, file = here::here("results", sprintf("centers2_%s.rds", config::get("scenario"))))


# Read data ---------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "2040_ve0")

zones <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))
centers <- readr::read_rds(here::here("results", "centers_uusimaa-2050.rds"))

ttimes_pt <- read_helmet_omx(file.path(config::get("helmet_data"),
                                       config::get("results"),
                                       "Matrices",
                                       "time_pt.omx")) %>%
  dplyr::filter(origin %in% zones$zone & destination %in% centers$SIJ2019) %>%
  dplyr::rename(car = car_work,
                transit = transit_work,
                bike = bike,
                walk = walk) %>%
  dplyr::select(origin, destination, car, transit, bike, walk)

ttimes <- ttimes_pt %>%
  tidyr::pivot_longer(
    cols = car:walk,
    values_to = "ttime",
    names_to = "mode"
  ) %>%
  dplyr::group_by(origin, mode) %>%
  dplyr::arrange(ttime) %>%
  dplyr::summarise(accessibility2 = sum(ttime[1:2]), .groups = "drop")

ttimes2 <- ttimes %>%
  dplyr::left_join(ttimes_range, by = "mode") %>%
  dplyr::mutate(
    accessibility2_normal =  (100 - 1) * (accessibility2 - amin) / (amax - amin) + 1
  ) %>%
  dplyr::mutate(
    accessibility2_normal = pmax(1, pmin(accessibility2_normal, 100))
  ) %>%
  tidyr::pivot_wider(
    id_cols = origin,
    names_from = mode,
    values_from = accessibility2_normal
  )


# Output ------------------------------------------------------------------

readr::write_rds(ttimes2, file = here::here("results", sprintf("centers2_%s.rds", config::get("scenario"))))
