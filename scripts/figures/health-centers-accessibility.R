# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(viridis)

source(here::here("scripts", "figures", "validate_scenarios.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

# Data --------------------------------------------------------------------

zones <- readr::read_rds(here::here("results", "zones.rds"))
health_centers <- readr::read_rds(
  here::here("results", "health-centers.rds")
) %>%
  dplyr::mutate(zone = as.character(zone))

stopifnot(inherits(zones, "sf"))
stopifnot(inherits(health_centers, "sf"))
stopifnot(all(sf::st_crs(zones) == sf::st_crs(health_centers)))

# Closest-facility travel time --------------------------------------------

zone_polygons <- zones %>%
  dplyr::mutate(zone = as.character(SIJ2023)) %>%
  dplyr::select(zone, geometry)

zone_centroids <- sf::st_centroid(zone_polygons)

facility_groups <- list(
  hospitals = list(
    label = "Sairaalat ja laboratoriot",
    purpose_names = c(
      "Keskussairaalat",
      "Erikoissairaalat ja laboratoriorakennukset",
      "Muut sairaalat"
    )
  ),
  other_health = list(
    label = "Terveys- ja hyvinvointikeskukset",
    purpose_names = c(
      "Muut terveydenhuoltorakennukset",
      "Terveys- ja hyvinvointikeskukset"
    )
  )
)

travel_modes <- c(
  car_work = "auto",
  transit_work = "joukkoliikenne"
)

read_travel_times <- function(scenario) {
  scenario_attributes <- set_scenario(scenario)

  read_helmet_omx(file.path(
    config::get("helmet_data"),
    scenario_attributes[["results"]],
    "Matrices",
    "time_pt.omx"
  )) %>%
    dplyr::select(origin, destination, car_work, transit_work) %>%
    dplyr::mutate(
      origin = as.character(origin),
      destination = as.character(destination)
    ) %>%
    dplyr::filter(
      origin %in% zone_centroids$zone,
      destination %in% health_centers$zone
    )
}

calculate_closest_travel_time <- function(zone_centroids, facilities,
                                          travel_times, travel_mode) {
  zone_ids <- as.character(zone_centroids$zone)
  facility_zones <- unique(as.character(facilities$zone))

  if (length(facility_zones) == 0) {
    rlang::abort("The selected facility group contains no facilities.")
  }

  zone_travel_times <- travel_times %>%
    dplyr::filter(
      origin %in% zone_ids,
      destination %in% facility_zones
    )
  unique_zone_travel_times <- matrix(
    NA_real_,
    nrow = length(zone_ids),
    ncol = length(facility_zones)
  )
  travel_time_indices <- cbind(
    match(zone_travel_times$origin, zone_ids),
    match(zone_travel_times$destination, facility_zones)
  )
  selected_travel_times <- zone_travel_times[[travel_mode]]

  if (anyNA(selected_travel_times) || any(!is.finite(selected_travel_times))) {
    rlang::abort(
      sprintf("%s travel times are missing for one or more zone pairs.", travel_mode)
    )
  }
  if (any(selected_travel_times < 0)) {
    rlang::abort(sprintf("%s travel times cannot be negative.", travel_mode))
  }

  unique_zone_travel_times[travel_time_indices] <- selected_travel_times
  if (anyNA(unique_zone_travel_times) || any(!is.finite(unique_zone_travel_times))) {
    rlang::abort(
      sprintf("%s travel times are missing for one or more zone pairs.", travel_mode)
    )
  }

  facility_indices <- match(as.character(facilities$zone), facility_zones)
  travel_time_matrix <- unique_zone_travel_times[, facility_indices, drop = FALSE]

  zone_centroids %>%
    dplyr::mutate(
      closest_travel_time_min = apply(travel_time_matrix, 1, min)
    )
}

travel_time_scenarios <- c("2045_ve0", "2045_suunnitelma")
travel_times_by_scenario <- travel_time_scenarios %>%
  purrr::set_names() %>%
  purrr::map(read_travel_times)

accessibility_results <- purrr::map_dfr(
  names(facility_groups),
  function(facility_group) {
    group <- facility_groups[[facility_group]]
    facilities <- health_centers %>%
      dplyr::filter(.data$purpose_name %in% group$purpose_names)

    purrr::map_dfr(
      travel_time_scenarios,
      function(scenario) {
        purrr::map_dfr(
          names(travel_modes),
          function(travel_mode) {
            calculate_closest_travel_time(
              zone_centroids,
              facilities,
              travel_times_by_scenario[[scenario]],
              travel_mode
            ) %>%
              dplyr::mutate(
                facility_group = facility_group,
                facility_group_label = group$label,
                travel_time_scenario = scenario,
                travel_mode = travel_mode
              )
          }
        )
      }
    )
  }
)

readr::write_rds(
  accessibility_results,
  file = here::here("results", "health-centers-accessibility.rds")
)

# Maps --------------------------------------------------------------------

figure_directory <- here::here("figures", "health-centers")
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

plot_travel_time <- function(data, title, subtitle, fill_column) {
  ggplot() +
    geom_sf(
      mapping = aes(fill = .data[[fill_column]]),
      data = data,
      color = "#D0CCC0",
      linewidth = 0.08
    ) +
    scale_fill_viridis(
      option = "magma",
      direction = 1,
      limits = c(0, 60),
      oob = scales::squish,
      name = "matka-aika\n(min)",
      labels = scales::label_number(accuracy = 1)
    ) +
    geom_basemap() +
    coord_sf_mal() +
    annotate_map(title = title, subtitle = subtitle) +
    theme_mal_map()
}

for (facility_group in names(facility_groups)) {
  group <- facility_groups[[facility_group]]

  for (travel_mode in names(travel_modes)) {
    scenario_results <- accessibility_results %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(
        .data$facility_group == .env$facility_group,
        .data$travel_mode == .env$travel_mode
      ) %>%
      dplyr::select(
        zone,
        travel_time_scenario,
        closest_travel_time_min
      )

    plan_data <- zone_polygons %>%
      dplyr::left_join(
        scenario_results %>%
          dplyr::filter(.data$travel_time_scenario == "2045_suunnitelma") %>%
          dplyr::select(
            zone,
            plan_travel_time_min = closest_travel_time_min
          ),
        by = "zone"
      )

    plan_plot <- plot_travel_time(
      plan_data,
      title = paste("Lähimmän kohteen matka-aika:", group$label),
      subtitle = paste(
        "2045 LJS-suunnitelma,",
        travel_modes[[travel_mode]]
      ),
      fill_column = "plan_travel_time_min"
    )
    ggsave_map(
      here::here(
        "figures", "health-centers",
        sprintf(
          "map_health-centers-accessibility_%s_%s.png",
          facility_group,
          travel_mode
        )
      ),
      plot = plan_plot
    )

    comparison_data <- zone_polygons %>%
      dplyr::left_join(
        scenario_results %>%
          dplyr::filter(.data$travel_time_scenario == "2045_ve0") %>%
          dplyr::select(
            zone,
            baseline_travel_time_min = closest_travel_time_min
          ),
        by = "zone"
      ) %>%
      dplyr::left_join(
        scenario_results %>%
          dplyr::filter(.data$travel_time_scenario == "2045_suunnitelma") %>%
          dplyr::select(
            zone,
            plan_travel_time_min = closest_travel_time_min
          ),
        by = "zone"
      ) %>%
      dplyr::mutate(
        travel_time_difference_min = (
          plan_travel_time_min - baseline_travel_time_min
        )
      )

    difference_limit <- if (facility_group == "hospitals") 60 else 30

    difference_plot <- ggplot() +
      geom_sf(
        mapping = aes(fill = .data$travel_time_difference_min),
        data = comparison_data,
        color = "#D0CCC0",
        linewidth = 0.08
      ) +
      scale_fill_gradient2(
        low = "#3E8606",
        mid = "white",
        high = "#7b1154",
        midpoint = 0,
        limits = c(-difference_limit, difference_limit),
        oob = scales::squish,
        name = "matka-ajan\nmuutos (min)",
        labels = scales::label_number(accuracy = 1)
      ) +
      geom_basemap() +
      coord_sf_mal() +
      annotate_map(
        title = paste("LJS-suunnitelman vaikutus:", group$label),
        subtitle = paste(
          "Matka-ajan muutos (min), 2045 LJS-suunnitelma - 2045 vertailupohja,",
          travel_modes[[travel_mode]]
        )
      ) +
      theme_mal_map()

    ggsave_map(
      here::here(
        "figures", "health-centers",
        sprintf(
          "map_health-centers-accessibility-diff_%s_%s.png",
          facility_group,
          travel_mode
        )
      ),
      plot = difference_plot
    )
  }
}
