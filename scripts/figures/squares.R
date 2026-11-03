# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

squares <- here::here("data", "Tavoiteverkkojen yhdistetty maankäyttöaineisto",
                          "aineistopyynto_siuntio_ruudukko_yhdistetty.gpkg") %>%
  sf::read_sf() %>%
  sf::st_transform(3879)

# Use an internal row identifier to preserve alignment during spatial matching.
# The input baseline is 2024; keep the historical 2023 column names for consumers.
squares <- squares %>%
  tibble::rownames_to_column(var = "xyind") %>%
  dplyr::mutate(xyind = as.integer(xyind)) %>%
  dplyr::select(xyind, asukkaat_24, asukkaat_45) %>%
  dplyr::rename(asuk23 = asukkaat_24,
                asuk45 = asukkaat_45)

# If you need the row index as a character instead, use:
# squares <- squares %>%
#   tibble::rownames_to_column(var = "xyind") %>%
#   dplyr::mutate(xyind = as.character(xyind))

# squares <- squares %>%
#   dplyr::select(xyind, asukkaat_24, asukkaat_45) %>%
#   dplyr::rename(asuk24 = asukkaat_24,
#                 asuk45 = asukkaat_45)

zones <- readr::read_rds(here::here("results", "zones.rds")) %>%
  dplyr::select(SIJ2023) %>%
  dplyr::rename(zone = SIJ2023)

squares <- squares %>%
  dplyr::mutate(
    pop_diff_2023_2045_ve0 = asuk45 - asuk23,
    pop_diff_2023_2045_tavoiteverkko = asuk45 - asuk23,
    pop_diff_2023_2045_suunnitelma = asuk45 - asuk23
  ) %>%
  dplyr::mutate(
    pop_increase_2023_2045_ve0 = pmax(pop_diff_2023_2045_ve0, 0.0),
    pop_increase_2023_2045_tavoiteverkko = pmax(pop_diff_2023_2045_tavoiteverkko, 0.0),
    pop_increase_2023_2045_suunnitelma = pmax(pop_diff_2023_2045_suunnitelma, 0.0),
  )


# Data --------------------------------------------------------------------

# ensi <- readr::read_rds(here::here("results", "ensi_ruudut.rds")) %>%
#   sf::st_drop_geometry()

centers_and_stations <- readr::read_rds(here::here("results", "centers-and-stations.rds")) %>%
  sf::st_transform(sf::st_crs(squares))
zones <- sf::st_transform(zones, sf::st_crs(squares))

# Tavoiteverkko uses the ve1 basemap in scenarios.tsv.
if (!"station_2045_tavoiteverkko" %in% names(centers_and_stations)) {
  centers_and_stations <- centers_and_stations %>%
    dplyr::mutate(station_2045_tavoiteverkko = station_2045_ve1)
}

# Match each centroid once, combining flags if coverage polygons overlap.
squares_centroids <- squares %>%
  dplyr::select(xyind) %>%
  sf::st_centroid()
coverage_matches <- sf::st_intersects(squares_centroids, centers_and_stations)
coverage_columns <- c("center", "station_2023", "station_2045_ve0",
                      "station_2045_tavoiteverkko", "station_2045_suunnitelma")
stopifnot(all(coverage_columns %in% names(centers_and_stations)))
for (column in coverage_columns) {
  squares_centroids[[column]] <- vapply(coverage_matches, function(rows) {
    any(centers_and_stations[[column]][rows], na.rm = TRUE)
  }, logical(1))
}
squares_centroids <- squares_centroids %>%
  dplyr::mutate(center_or_station_2023 = center | station_2023,
                center_or_station_2045_ve0 = center | station_2045_ve0,
                center_or_station_2045_tavoiteverkko = center | station_2045_tavoiteverkko,
                center_or_station_2045_suunnitelma = center | station_2045_suunnitelma) %>%

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

stopifnot(nrow(squares_centroids) == nrow(squares),
          identical(squares_centroids$xyind, squares$xyind))

squares <- squares %>%
  dplyr::bind_cols(dplyr::select(squares_centroids, !xyind))

# Areas -------------------------------------------------------------------

# Keep areas with no covered squares, and use NA for undefined shares.
# Shares retain the existing net population change definition (including losses).
calc_areas <- function(squares, group) {
  data <- squares %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(.covered = {{ group }})
  stopifnot(!anyNA(data$area), !anyNA(data$.covered))
  population_columns <- c("pop_diff_2023_2045_ve0",
                          "pop_diff_2023_2045_tavoiteverkko",
                          "pop_diff_2023_2045_suunnitelma")
  stopifnot(all(vapply(data[population_columns],
                      function(x) all(is.finite(x)), logical(1))))

  aggregate_shares <- function(data) {
    data %>%
      dplyr::group_by(area) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(population_columns),
                      ~ if (sum(.x) == 0) NA_real_ else sum(.x[.covered]) / sum(.x)),
        .groups = "drop"
      ) %>%
      dplyr::rename_with(~ sub("pop_diff_2023_2045_", "value_", .x),
                         dplyr::all_of(population_columns))
  }

  capital <- data %>%
    dplyr::filter(area %in% c("helsinki_cbd", "helsinki_other", "espoo_vant_kau")) %>%
    dplyr::mutate(area = "capital_region")
  region <- data %>% dplyr::mutate(area = "helsinki_region")
  aggregate_shares(dplyr::bind_rows(data, capital, region))
}

#ensi <- calc_areas(squares, ensi)

centers <- calc_areas(squares, center)

centers_or_stations_2045_ve0 <- calc_areas(squares, center_or_station_2045_ve0)
stations_2045_ve0 <- calc_areas(squares, station_2045_ve0)

centers_or_stations_2045_tavoiteverkko <- calc_areas(squares, center_or_station_2045_tavoiteverkko)
stations_2045_tavoiteverkko <- calc_areas(squares, station_2045_tavoiteverkko)

centers_or_stations_2045_suunnitelma <- calc_areas(squares, center_or_station_2045_suunnitelma)
stations_2045_suunnitelma <- calc_areas(squares, station_2045_suunnitelma)





# ensi_ve0 <- ensi %>%
#   dplyr::select(area, dplyr::ends_with("ve0")) %>%
#   dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
#   dplyr::mutate(scenario = "2045_ve0") %>%
#   dplyr::rename(pop_share_ensi = value)

# ensi_ve1 <- ensi %>%
#   dplyr::select(area, dplyr::ends_with("ve1")) %>%
#   dplyr::rename_with(~ gsub("_ve1", "", .x)) %>%
#   dplyr::mutate(scenario = "2045_ve1") %>%
#   dplyr::rename(pop_share_ensi = value)

# ensi_ve2 <- ensi %>%
#   dplyr::select(area, dplyr::ends_with("ve2")) %>%
#   dplyr::rename_with(~ gsub("_ve2", "", .x)) %>%
#   dplyr::mutate(scenario = "2040_ve2") %>%
#   dplyr::rename(pop_share_ensi = value)

# ensi_suunnitelma <- ensi %>%
#   dplyr::select(area, dplyr::ends_with("suunnitelma")) %>%
#   dplyr::rename_with(~ gsub("_suunnitelma", "", .x)) %>%
#   dplyr::mutate(scenario = "2045_suunnitelma") %>%
#   dplyr::rename(pop_share_ensi = value)








centers_ve0 <- centers %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2045_ve0") %>%
  dplyr::rename(pop_share_center = value)

centers_tavoiteverkko <- centers %>%
  dplyr::select(area, dplyr::ends_with("tavoiteverkko")) %>%
  dplyr::rename_with(~ gsub("_tavoiteverkko", "", .x)) %>%
  dplyr::mutate(scenario = "2045_tavoiteverkko") %>%
  dplyr::rename(pop_share_center = value)

centers_suunnitelma <- centers %>%
  dplyr::select(area, dplyr::ends_with("suunnitelma")) %>%
  dplyr::rename_with(~ gsub("_suunnitelma", "", .x)) %>%
  dplyr::mutate(scenario = "2045_suunnitelma") %>%
  dplyr::rename(pop_share_center = value)






stations_ve0 <- stations_2045_ve0 %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2045_ve0") %>%
  dplyr::rename(pop_share_station = value)

stations_tavoiteverkko <- stations_2045_tavoiteverkko %>%
  dplyr::select(area, dplyr::ends_with("tavoiteverkko")) %>%
  dplyr::rename_with(~ gsub("_tavoiteverkko", "", .x)) %>%
  dplyr::mutate(scenario = "2045_tavoiteverkko") %>%
  dplyr::rename(pop_share_station = value)

stations_suunnitelma <- stations_2045_suunnitelma %>%
  dplyr::select(area, dplyr::ends_with("suunnitelma")) %>%
  dplyr::rename_with(~ gsub("_suunnitelma", "", .x)) %>%
  dplyr::mutate(scenario = "2045_suunnitelma") %>%
  dplyr::rename(pop_share_station = value)






centers_or_stations_ve0 <- centers_or_stations_2045_ve0 %>%
  dplyr::select(area, dplyr::ends_with("ve0")) %>%
  dplyr::rename_with(~ gsub("_ve0", "", .x)) %>%
  dplyr::mutate(scenario = "2045_ve0") %>%
  dplyr::rename(pop_share_center_or_station = value)

centers_or_stations_tavoiteverkko <- centers_or_stations_2045_tavoiteverkko %>%
  dplyr::select(area, dplyr::ends_with("tavoiteverkko")) %>%
  dplyr::rename_with(~ gsub("_tavoiteverkko", "", .x)) %>%
  dplyr::mutate(scenario = "2045_tavoiteverkko") %>%
  dplyr::rename(pop_share_center_or_station = value)

centers_or_stations_suunnitelma <- centers_or_stations_2045_suunnitelma %>%
  dplyr::select(area, dplyr::ends_with("suunnitelma")) %>%
  dplyr::rename_with(~ gsub("_suunnitelma", "", .x)) %>%
  dplyr::mutate(scenario = "2045_suunnitelma") %>%
  dplyr::rename(pop_share_center_or_station = value)

squares_areas_2045_ve0 <- centers_ve0 %>%
  # dplyr::left_join(centers_ve0, by = c("area", "scenario")) %>%
  dplyr::left_join(stations_ve0, by = c("area", "scenario")) %>%
  dplyr::left_join(centers_or_stations_ve0, by = c("area", "scenario"))

squares_areas_2045_tavoiteverkko <- centers_tavoiteverkko %>%
  dplyr::left_join(stations_tavoiteverkko, by = c("area", "scenario")) %>%
  dplyr::left_join(centers_or_stations_tavoiteverkko, by = c("area", "scenario"))

squares_areas_2045_suunnitelma <- centers_suunnitelma %>%
  dplyr::left_join(stations_suunnitelma, by = c("area", "scenario")) %>%
  dplyr::left_join(centers_or_stations_suunnitelma, by = c("area", "scenario"))



# Write only after all scenario calculations have succeeded.
readr::write_rds(squares, file = here::here("results", "squares.rds"))
squares %>% sf::write_sf(here::here("results", "squares.gpkg"), append = FALSE, delete_dsn = TRUE)
squares_areas_2045_ve0 %>% readr::write_rds(here::here("results", "squares_areas_2045_ve0.rds"))
squares_areas_2045_tavoiteverkko %>% readr::write_rds(here::here("results", "squares_areas_2045_tavoiteverkko.rds"))
squares_areas_2045_suunnitelma %>% readr::write_rds(here::here("results", "squares_areas_2045_suunnitelma.rds"))












# calc_areas_apartments <- function(squares, group) {
#   df <- squares %>%
#     sf::st_drop_geometry() %>%
#     dplyr::group_by(area, {{ group }}) %>%
#     dplyr::summarise(
#       apartments_diff_2022_2035 = sum(apartments_diff_2022_2035),
#       .groups = "drop_last"
#     ) %>%
#     dplyr::mutate(
#       value = apartments_diff_2022_2035 / sum(apartments_diff_2022_2035),
#       apartments_diff_2022_2035 = sum(apartments_diff_2022_2035),
#     ) %>%
#     dplyr::filter({{ group }}) %>%
#     dplyr::select(!{{group}})

#   df_all <- squares %>%
#     sf::st_drop_geometry() %>%
#     dplyr::group_by({{ group }}) %>%
#     dplyr::summarise(
#       apartments_diff_2022_2035 = sum(apartments_diff_2022_2035),
#       .groups = "drop_last"
#     ) %>%
#     dplyr::mutate(
#       value = apartments_diff_2022_2035 / sum(apartments_diff_2022_2035),
#       apartments_diff_2022_2035 = sum(apartments_diff_2022_2035),
#     ) %>%
#     dplyr::filter({{ group }}) %>%
#     dplyr::select(!{{group}}) %>%
#     dplyr::mutate(area = "helsinki_region")

#   return(dplyr::bind_rows(df, df_all))
# }

# apartments <- calc_areas_apartments(squares, ensi) %>%
#   dplyr::rename(apartment_share_ensi = value)

# apartments %>% readr::write_rds(here::here("results", "squares_areas_ensi_apartments.rds"))




