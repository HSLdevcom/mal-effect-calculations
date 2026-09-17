# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)

source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

# Data --------------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))
health_centers <- readr::read_rds(
  here::here("results", "health-centers.rds")
)

stopifnot(inherits(zones, "sf"))
stopifnot(inherits(health_centers, "sf"))
stopifnot(all(sf::st_crs(zones) == sf::st_crs(health_centers)))

# Accessibility index ----------------------------------------------------

# The available health-centre data contains locations but no modelled travel
# times. The index therefore uses a distance-decay score: every facility
# contributes to accessibility, with nearby facilities contributing more.
zone_polygons <- zones %>%
  dplyr::mutate(zone = SIJ2023) %>%
  dplyr::select(zone, geometry)

zone_centroids <- zone_polygons %>%
  sf::st_centroid()

calculate_accessibility <- function(zone_centroids, facilities, status,
                                    decay_distance_m = 10000) {
  distances <- sf::st_distance(zone_centroids, facilities) %>%
    as.numeric()
  distances <- matrix(
    distances,
    nrow = nrow(zone_centroids),
    ncol = nrow(facilities)
  )
  accessibility_score <- rowSums(exp(-distances / decay_distance_m))
  score_range <- range(accessibility_score, na.rm = TRUE)

  if (diff(score_range) == 0) {
    accessibility_index <- rep(100, length(accessibility_score))
  } else {
    accessibility_index <- scales::rescale(
      accessibility_score,
      to = c(0, 100)
    )
  }

  zone_centroids %>%
    dplyr::mutate(
      health_center_accessibility_score = accessibility_score,
      health_center_accessibility = accessibility_index,
      facility_set = status
    )
}

facility_sets <- list(
  current = health_centers %>% dplyr::filter(status == "Nykyinen"),
  planned = health_centers %>% dplyr::filter(status == "Suunniteltu"),
  all = health_centers
)

accessibility_results <- purrr::imap(
  facility_sets,
  ~ calculate_accessibility(zone_centroids, .x, .y)
)

readr::write_rds(
  do.call(rbind, accessibility_results),
  file = here::here("results", "health-centers-accessibility.rds")
)

# Maps --------------------------------------------------------------------

plot_accessibility <- function(data, title, subtitle) {
  ggplot() +
    geom_sf(
      mapping = aes(fill = health_center_accessibility),
      data = data,
      color = "#D0CCC0",
      linewidth = 0.08
    ) +
    scale_fill_viridis(
      option = "magma",
      name = "indeksi",
      labels = scales::label_number(accuracy = 1),
      limits = c(0, 100),
      direction = -1,
      oob = scales::squish
    ) +
    geom_basemap() +
    coord_sf_mal() +
    annotate_map(title = title, subtitle = subtitle) +
    theme_mal_map()
}

map_titles <- c(
  current = "Terveyskeskusten saavutettavuus nykytilanteessa",
  planned = "Terveyskeskusten saavutettavuus suunnitelluilla kohteilla",
  all = "Terveyskeskusten saavutettavuus nykyisillä ja suunnitelluilla kohteilla"
)

purrr::walk(
  names(facility_sets),
  function(facility_set) {
    data <- zone_polygons %>%
      dplyr::left_join(
        accessibility_results[[facility_set]] %>%
          sf::st_drop_geometry() %>%
          dplyr::select(zone, health_center_accessibility_score,
                        health_center_accessibility),
        by = "zone"
      )

    plot <- plot_accessibility(
      data,
      title = map_titles[[facility_set]],
      subtitle = "Etäisyyksillä painotettu saavutettavuus TAZ-alueen keskipisteestä"
    )

    ggsave_map(
      here::here("figures", sprintf("map_health-centers-accessibility_%s.png", facility_set)),
      plot = plot
    )
  }
)