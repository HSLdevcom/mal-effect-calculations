# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

squares <- here::here("data", "Maankaytto",
                      "MAL2023_vertailupohja_YKR_250m_20220210",
                      "MAL2023_vertailupohja_YKR_250m_20220210.shp") %>%
  sf::read_sf() %>%
  sf::st_set_crs(3067) %>%
  sf::st_transform(3879)

zones <- readr::read_rds(here::here("results", "zones.rds")) %>%
  dplyr::select(SIJ2019) %>%
  dplyr::rename(zone = SIJ2019)

squares2 <- squares %>%
  sf::st_join(zones, join = sf::st_nearest_feature) %>%
  dplyr::mutate(
    area = dplyr::case_when(
      zone %in% 0:999 ~ "helsinki_cbd",
      zone %in% 1000:1999 ~ "helsinki_other",
      zone %in% 2000:5999 ~ "espoo_vant_kau",
      zone %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999) ~ "surround_train",
      zone %in% c(7000:9999, 12000:12999, 15000:15499) ~ "surround_other",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::mutate(
    pop_diff_2020_2040_ve0 = asuk40yht - asuk20YKR,
    floor_area_diff_2021_2040_ve0 = aska2140
  ) %>%
  dplyr::select(zone, area, pop_diff_2020_2040_ve0, floor_area_diff_2021_2040_ve0) %>%
  dplyr::mutate(
    pop_increase_2020_2040_ve0 = pmax(pop_diff_2020_2040_ve0, 0.0),
    floor_area_increase_2021_2040_ve0 = pmax(floor_area_diff_2021_2040_ve0, 0.0)
  )

ensi <- readr::read_rds(here::here("results", "ensi.rds"))
uml <- readr::read_rds(here::here("results", "uml.rds"))
# TODO: Add train and metro stations

squares2 <- squares2 %>%
  sf::st_join(ensi) %>%
  sf::st_join(uml) %>%
  dplyr::mutate(ensi = tidyr::replace_na(ensi, FALSE))

readr::write_rds(squares2, file = here::here("results", "squares.rds"))
