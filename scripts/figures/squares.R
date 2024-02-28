# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

squares <- here::here("data", "MAL2023_asukkaat_Ve2_1_20221010",
                          "MAL2023_asukkaat_Ve2_1_20221010_region.shp") %>%
  sf::read_sf() %>%
  sf::st_transform(3879) %>%
  dplyr::select(xyind, asuk20YKR, asuk40uV00, asuk40Ve00, asyht_22_3) %>%
  dplyr::rename(asuk20 = asuk20YKR,
                asuk40ve0 = asuk40uV00,
                asuk40ve2 = asuk40Ve00,
                apartments_diff_2022_2035 = asyht_22_3)

zones <- readr::read_rds(here::here("results", "zones.rds")) %>%
  dplyr::select(SIJ2019) %>%
  dplyr::rename(zone = SIJ2019)

squares <- squares %>%
  dplyr::mutate(
    pop_diff_2020_2040_ve0 = asuk40ve0 - asuk20,
    # pop_diff_2020_2040_ve1 = asuk40ve1 - asuk20,
    pop_diff_2020_2040_ve2 = asuk40ve2 - asuk20,
    pop_diff_2020_2040_suunnitelma = pop_diff_2020_2040_ve2
  ) %>%
  dplyr::mutate(
    pop_increase_2020_2040_ve0 = pmax(pop_diff_2020_2040_ve0, 0.0),
    # pop_increase_2020_2040_ve1 = pmax(pop_diff_2020_2040_ve1, 0.0),
    pop_increase_2020_2040_ve2 = pmax(pop_diff_2020_2040_ve2, 0.0),
    pop_increase_2020_2040_suunnitelma = pop_increase_2020_2040_ve2
  )


# Data --------------------------------------------------------------------

ensi <- readr::read_rds(here::here("results", "ensi_ruudut.rds")) %>%
  sf::st_drop_geometry()

centers_and_stations <- readr::read_rds(here::here("results", "centers-and-stations.rds"))

# Let us drop square polygons for a while and focus on centroids that are much easier to join into areas.
squares_centroids <- squares %>%
  dplyr::select(xyind) %>%
  sf::st_centroid() %>%
  dplyr::left_join(ensi, by = "xyind") %>%
  sf::st_join(centers_and_stations) %>%
  tidyr::replace_na(list(ensi = FALSE,
                         center = FALSE,
                         station_2018 = FALSE,
                         station_2040_ve0 = FALSE,
                         # station_2040_ve1 = FALSE,
                         station_2040_ve2 = FALSE,
                         station_2040_suunnitelma = FALSE)) %>%
  dplyr::mutate(center_or_station_2018 = center | station_2018,
                center_or_station_2040_ve0 = center | station_2040_ve0,
                # center_or_station_2040_ve1 = center | station_2040_ve1,
                center_or_station_2040_ve2 = center | station_2040_ve2,
                center_or_station_2040_suunnitelma = center | station_2040_suunnitelma) %>%
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
  sf::st_drop_geometry()

stopifnot(all(squares_centroids$xyind == squares$xyind))

squares <- squares %>%
  dplyr::bind_cols(dplyr::select(squares_centroids, !xyind))

readr::write_rds(squares, file = here::here("results", "squares.rds"))
squares %>% sf::write_sf(here::here("results", "squares.gpkg"), append = FALSE, delete_dsn = TRUE)


# Areas -------------------------------------------------------------------

calc_areas <- function(squares, group) {
  df <- squares %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(area, {{ group }}) %>%
    dplyr::summarise(
      pop_diff_2020_ve0 = sum(pop_diff_2020_2040_ve0),
      # pop_diff_2020_ve1 = sum(pop_diff_2020_2040_ve1),
      pop_diff_2020_ve2 = sum(pop_diff_2020_2040_ve2),
      pop_diff_2020_suunnitelma = sum(pop_diff_2020_2040_suunnitelma),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(
      value_ve0 = pop_diff_2020_ve0 / sum(pop_diff_2020_ve0),
      # value_ve1 = pop_diff_2020_ve1 / sum(pop_diff_2020_ve1),
      value_ve2 = pop_diff_2020_ve2 / sum(pop_diff_2020_ve2),
      value_suunnitelma = pop_diff_2020_suunnitelma / sum(pop_diff_2020_suunnitelma),
      pop_diff_2020_ve0 = sum(pop_diff_2020_ve0),
      # pop_diff_2020_ve1 = sum(pop_diff_2020_ve1),
      pop_diff_2020_ve2 = sum(pop_diff_2020_ve2),
      pop_diff_2020_suunnitelma = sum(pop_diff_2020_suunnitelma),
    ) %>%
    dplyr::filter({{ group }}) %>%
    dplyr::select(!{{group}})

  df_all <- squares %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::summarise(
      pop_diff_2020_ve0 = sum(pop_diff_2020_2040_ve0),
      # pop_diff_2020_ve1 = sum(pop_diff_2020_2040_ve1),
      pop_diff_2020_ve2 = sum(pop_diff_2020_2040_ve2),
      pop_diff_2020_suunnitelma = sum(pop_diff_2020_2040_suunnitelma),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(
      value_ve0 = pop_diff_2020_ve0 / sum(pop_diff_2020_ve0),
      # value_ve1 = pop_diff_2020_ve1 / sum(pop_diff_2020_ve1),
      value_ve2 = pop_diff_2020_ve2 / sum(pop_diff_2020_ve2),
      value_suunnitelma = pop_diff_2020_suunnitelma / sum(pop_diff_2020_suunnitelma),
      pop_diff_2020_ve0 = sum(pop_diff_2020_ve0),
      # pop_diff_2020_ve1 = sum(pop_diff_2020_ve1),
      pop_diff_2020_ve2 = sum(pop_diff_2020_ve2),
      pop_diff_2020_suunnitelma = sum(pop_diff_2020_suunnitelma),
    ) %>%
    dplyr::filter({{ group }}) %>%
    dplyr::select(!{{group}}) %>%
    dplyr::mutate(area = "helsinki_region")

  return(dplyr::bind_rows(df, df_all))
}

ensi <- calc_areas(squares, ensi)

centers <- calc_areas(squares, center)

centers_or_stations_2040_ve0 <- calc_areas(squares, center_or_station_2040_ve0)
stations_2040_ve0 <- calc_areas(squares, station_2040_ve0)

# centers_or_stations_2040_ve1 <- calc_areas(squares, center_or_station_2040_ve1)
# stations_2040_ve1 <- calc_areas(squares, station_2040_ve1)

centers_or_stations_2040_ve2 <- calc_areas(squares, center_or_station_2040_ve2)
stations_2040_ve2 <- calc_areas(squares, station_2040_ve2)

centers_or_stations_2040_suunnitelma <- calc_areas(squares, center_or_station_2040_suunnitelma)
stations_2040_suunnitelma <- calc_areas(squares, station_2040_suunnitelma)





ensi_ve0 <- ensi %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve0") %>%
  dplyr::rename(pop_share_ensi = value)

# ensi_ve1 <- ensi %>%
#   dplyr::select(area, dplyr::ends_with("ve1")) %>%
#   dplyr::rename_with(~ gsub("_ve1", "", .x)) %>%
#   dplyr::mutate(scenario = "2040_ve1") %>%
#   dplyr::rename(pop_share_ensi = value)

ensi_ve2 <- ensi %>%
  dplyr::select(area, dplyr::ends_with("ve2")) %>%
  dplyr::rename_with(~ gsub("_ve2", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve2") %>%
  dplyr::rename(pop_share_ensi = value)

ensi_suunnitelma <- ensi %>%
  dplyr::select(area, dplyr::ends_with("suunnitelma")) %>%
  dplyr::rename_with(~ gsub("_suunnitelma", "", .x)) %>%
  dplyr::mutate(scenario = "2040_suunnitelma") %>%
  dplyr::rename(pop_share_ensi = value)








centers_ve0 <- centers %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve0") %>%
  dplyr::rename(pop_share_center = value) %>%
  dplyr::select(!pop_diff_2020)

# centers_ve1 <- centers %>%
#   dplyr::select(area, dplyr::ends_with("ve1")) %>%
#   dplyr::rename_with(~ gsub("_ve1", "", .x)) %>%
#   dplyr::mutate(scenario = "2040_ve1") %>%
#   dplyr::rename(pop_share_center = value) %>%
#   dplyr::select(!pop_diff_2020)

centers_ve2 <- centers %>%
  dplyr::select(area, dplyr::ends_with("ve2")) %>%
  dplyr::rename_with(~ gsub("_ve2", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve2") %>%
  dplyr::rename(pop_share_center = value) %>%
  dplyr::select(!pop_diff_2020)

centers_suunnitelma <- centers %>%
  dplyr::select(area, dplyr::ends_with("suunnitelma")) %>%
  dplyr::rename_with(~ gsub("_suunnitelma", "", .x)) %>%
  dplyr::mutate(scenario = "2040_suunnitelma") %>%
  dplyr::rename(pop_share_center = value) %>%
  dplyr::select(!pop_diff_2020)






stations_ve0 <- stations_2040_ve0 %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve0") %>%
  dplyr::rename(pop_share_station = value) %>%
  dplyr::select(!pop_diff_2020)

# stations_ve1 <- stations_2040_ve1 %>%
#   dplyr::select(area, dplyr::ends_with("ve1")) %>%
#   dplyr::rename_with(~ gsub("_ve1", "", .x)) %>%
#   dplyr::mutate(scenario = "2040_ve1") %>%
#   dplyr::rename(pop_share_station = value) %>%
#   dplyr::select(!pop_diff_2020)

stations_ve2 <- stations_2040_ve2 %>%
  dplyr::select(area, dplyr::ends_with("ve2")) %>%
  dplyr::rename_with(~ gsub("_ve2", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve2") %>%
  dplyr::rename(pop_share_station = value) %>%
  dplyr::select(!pop_diff_2020)

stations_suunnitelma <- stations_2040_suunnitelma %>%
  dplyr::select(area, dplyr::ends_with("suunnitelma")) %>%
  dplyr::rename_with(~ gsub("_suunnitelma", "", .x)) %>%
  dplyr::mutate(scenario = "2040_suunnitelma") %>%
  dplyr::rename(pop_share_station = value) %>%
  dplyr::select(!pop_diff_2020)






centers_or_stations_ve0 <- centers_or_stations_2040_ve0 %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve0") %>%
  dplyr::rename(pop_share_center_or_station = value) %>%
  dplyr::select(!pop_diff_2020)

# centers_or_stations_ve1 <- centers_or_stations_2040_ve1 %>%
#   dplyr::select(area, dplyr::ends_with("ve1")) %>%
#   dplyr::rename_with(~ gsub("_ve1", "", .x)) %>%
#   dplyr::mutate(scenario = "2040_ve1") %>%
#   dplyr::rename(pop_share_center_or_station = value) %>%
#   dplyr::select(!pop_diff_2020)

centers_or_stations_ve2 <- centers_or_stations_2040_ve2 %>%
  dplyr::select(area, dplyr::ends_with("ve2")) %>%
  dplyr::rename_with(~ gsub("_ve2", "", .x)) %>%
  dplyr::mutate(scenario = "2040_ve2") %>%
  dplyr::rename(pop_share_center_or_station = value) %>%
  dplyr::select(!pop_diff_2020)

centers_or_stations_suunnitelma <- centers_or_stations_2040_suunnitelma %>%
  dplyr::select(area, dplyr::ends_with("suunnitelma")) %>%
  dplyr::rename_with(~ gsub("_suunnitelma", "", .x)) %>%
  dplyr::mutate(scenario = "2040_suunnitelma") %>%
  dplyr::rename(pop_share_center_or_station = value) %>%
  dplyr::select(!pop_diff_2020)













squares_areas_2040_ve0 <- ensi_ve0 %>%
  dplyr::left_join(centers_ve0, by = c("area", "scenario")) %>%
  dplyr::left_join(stations_ve0, by = c("area", "scenario")) %>%
  dplyr::left_join(centers_or_stations_ve0, by = c("area", "scenario"))

# squares_areas_2040_ve1 <- ensi_ve1 %>%
#   dplyr::left_join(centers_ve1, by = c("area", "scenario")) %>%
#   dplyr::left_join(stations_ve1, by = c("area", "scenario")) %>%
#   dplyr::left_join(centers_or_stations_ve1, by = c("area", "scenario"))

squares_areas_2040_ve2 <- ensi_ve2 %>%
  dplyr::left_join(centers_ve2, by = c("area", "scenario")) %>%
  dplyr::left_join(stations_ve2, by = c("area", "scenario")) %>%
  dplyr::left_join(centers_or_stations_ve2, by = c("area", "scenario"))

squares_areas_2040_suunnitelma <- ensi_suunnitelma %>%
  dplyr::left_join(centers_suunnitelma, by = c("area", "scenario")) %>%
  dplyr::left_join(stations_suunnitelma, by = c("area", "scenario")) %>%
  dplyr::left_join(centers_or_stations_suunnitelma, by = c("area", "scenario"))



squares_areas_2040_ve0 %>% readr::write_rds(here::here("results", "squares_areas_2040_ve0.rds"))
# squares_areas_2040_ve1 %>% readr::write_rds(here::here("results", "squares_areas_2040_ve1u.rds"))
squares_areas_2040_ve2 %>% readr::write_rds(here::here("results", "squares_areas_2040_ve2.rds"))
squares_areas_2040_suunnitelma %>% readr::write_rds(here::here("results", "squares_areas_2040_suunnitelma.rds"))












calc_areas_apartments <- function(squares, group) {
  df <- squares %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(area, {{ group }}) %>%
    dplyr::summarise(
      apartments_diff_2022_2035 = sum(apartments_diff_2022_2035),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(
      value = apartments_diff_2022_2035 / sum(apartments_diff_2022_2035),
      apartments_diff_2022_2035 = sum(apartments_diff_2022_2035),
    ) %>%
    dplyr::filter({{ group }}) %>%
    dplyr::select(!{{group}})

  df_all <- squares %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::summarise(
      apartments_diff_2022_2035 = sum(apartments_diff_2022_2035),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(
      value = apartments_diff_2022_2035 / sum(apartments_diff_2022_2035),
      apartments_diff_2022_2035 = sum(apartments_diff_2022_2035),
    ) %>%
    dplyr::filter({{ group }}) %>%
    dplyr::select(!{{group}}) %>%
    dplyr::mutate(area = "helsinki_region")

  return(dplyr::bind_rows(df, df_all))
}

apartments <- calc_areas_apartments(squares, ensi) %>%
  dplyr::rename(apartment_share_ensi = value)

apartments %>% readr::write_rds(here::here("results", "squares_areas_ensi_apartments.rds"))




