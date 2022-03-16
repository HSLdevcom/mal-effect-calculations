# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

squares_ve0 <- here::here("data", "MAL2023_vertailupohja_YKR_250m_20220210_kaikkiruudut",
                          "MAL2023_vertailupohja_YKR_250m_20220210_kaikkiruudut.shp") %>%
  sf::read_sf() %>%
  sf::st_transform(3879) %>%
  dplyr::select(xyind, aska2140, asuk20YKR, asuk40yht) %>%
  dplyr::rename(asuk20ve0 = asuk20YKR,
                asuk40ve0 = asuk40yht)

squares_ve1 <- here::here("data", "MAL2023_Ve1_data_ja_info",
                          "MAL2023_maankäyttö_Ve1.shp") %>%
  sf::read_sf() %>%
  sf::st_transform(3879) %>%
  dplyr::select(xyind, asuk20, asuk40) %>%
  dplyr::rename(asuk20ve1 = asuk20,
                asuk40ve1 = asuk40) %>%
  sf::st_drop_geometry()

zones <- readr::read_rds(here::here("results", "zones.rds")) %>%
  dplyr::select(SIJ2019) %>%
  dplyr::rename(zone = SIJ2019)

squares <- squares_ve0 %>%
  dplyr::full_join(squares_ve1, by = "xyind")

message(sprintf("number of squares missing from ve0: %d", sum(is.na(squares$asuk40ve0))))

squares <- squares %>%
  dplyr::filter(!is.na(asuk40ve0)) %>%
  dplyr::mutate(
    asuk20 = dplyr::if_else(is.na(asuk20ve1), asuk20ve0, asuk20ve1),
    asuk40ve0 = tidyr::replace_na(asuk40ve0, 0),
    asuk40ve1 = tidyr::replace_na(asuk40ve1, 0),
    aska2140 = tidyr::replace_na(aska2140, 0),
  ) %>%
  dplyr::select(!c(asuk20ve0, asuk20ve1)) %>%
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
    pop_diff_2020_2040_ve0 = asuk40ve0 - asuk20,
    pop_diff_2020_2040_ve1 = asuk40ve1 - asuk20,
    floor_area_diff_2021_2040_ve0 = aska2140
  ) %>%
  dplyr::mutate(
    pop_increase_2020_2040_ve0 = pmax(pop_diff_2020_2040_ve0, 0.0),
    pop_increase_2020_2040_ve1 = pmax(pop_diff_2020_2040_ve1, 0.0),
    floor_area_increase_2021_2040_ve0 = pmax(floor_area_diff_2021_2040_ve0, 0.0)
  )

ensi <- readr::read_rds(here::here("results", "ensi_ruudut.rds")) %>%
  sf::st_drop_geometry()

uml <- readr::read_rds(here::here("results", "centers-and-stations.rds")) %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(center = TRUE) %>%
  dplyr::select(center)

squares <- squares %>%
  dplyr::left_join(ensi, by = "xyind") %>%
  sf::st_join(uml) %>%
  dplyr::mutate(ensi = tidyr::replace_na(ensi, FALSE),
                center = tidyr::replace_na(center, FALSE))

readr::write_rds(squares, file = here::here("results", "squares.rds"))


# Areas -------------------------------------------------------------------

calc_areas <- function(squares, group) {
  df <- squares %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(area, {{ group }}) %>%
    dplyr::summarise(
      pop_diff_2020_ve0 = sum(pop_diff_2020_2040_ve0),
      pop_diff_2020_ve1 = sum(pop_diff_2020_2040_ve1),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(
      value_ve0 = pop_diff_2020_ve0 / sum(pop_diff_2020_ve0),
      value_ve1 = pop_diff_2020_ve1 / sum(pop_diff_2020_ve1),
      pop_diff_2020_ve0 = sum(pop_diff_2020_ve0),
      pop_diff_2020_ve1 = sum(pop_diff_2020_ve1),
    ) %>%
    dplyr::filter({{ group }}) %>%
    dplyr::select(!{{group}})

  df_all <- squares %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::summarise(
      pop_diff_2020_ve0 = sum(pop_diff_2020_2040_ve0),
      pop_diff_2020_ve1 = sum(pop_diff_2020_2040_ve1),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(
      value_ve0 = pop_diff_2020_ve0 / sum(pop_diff_2020_ve0),
      value_ve1 = pop_diff_2020_ve1 / sum(pop_diff_2020_ve1),
      pop_diff_2020_ve0 = sum(pop_diff_2020_ve0),
      pop_diff_2020_ve1 = sum(pop_diff_2020_ve1),
    ) %>%
    dplyr::filter({{ group }}) %>%
    dplyr::select(!{{group}}) %>%
    dplyr::mutate(area = "helsinki_region")

  return(dplyr::bind_rows(df, df_all))
}

ensi <- calc_areas(squares, ensi)
centers <- calc_areas(squares, center)

ensi_ve0 <- ensi %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve0") %>%
  dplyr::rename(pop_share_ensi = value)

ensi_ve1 <- ensi %>%
  dplyr::select(area, dplyr::ends_with("ve1")) %>%
  dplyr::rename_with(~ gsub("_ve1", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve1") %>%
  dplyr::rename(pop_share_ensi = value)

centers_ve0 <- centers %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve0") %>%
  dplyr::rename(pop_share_center = value) %>%
  dplyr::select(!pop_diff_2020)

centers_ve1 <- centers %>%
  dplyr::select(area, dplyr::ends_with("ve1")) %>%
  dplyr::rename_with(~ gsub("_ve1", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve1") %>%
  dplyr::rename(pop_share_center = value) %>%
  dplyr::select(!pop_diff_2020)

squares_areas_2040_ve0 <- ensi_ve0 %>%
  dplyr::left_join(centers_ve0, by = c("area", "scenario"))

squares_areas_2040_ve1 <- ensi_ve1 %>%
  dplyr::left_join(centers_ve1, by = c("area", "scenario"))

squares_areas_2040_ve0 %>% readr::write_rds(here::here("results", "squares_areas_2040_ve0.rds"))
squares_areas_2040_ve1 %>% readr::write_rds(here::here("results", "squares_areas_2040_ve1.rds"))
