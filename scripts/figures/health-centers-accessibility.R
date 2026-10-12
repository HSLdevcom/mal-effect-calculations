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

# Accessibility index ----------------------------------------------------

# Each facility contributes to the weighted score, while the closest-facility
# measure uses only the minimum travel time. Indices are normalized within each
# travel mode across all three scenarios so baseline and plan can be compared.
zone_polygons <- zones %>%
  dplyr::mutate(zone = as.character(SIJ2023)) %>%
  dplyr::select(zone, geometry)

zone_centroids <- sf::st_centroid(zone_polygons)

read_travel_times <- function(scenario) {
  scenario_attributes <- set_scenario(scenario)

  read_helmet_omx(file.path(
    config::get("helmet_data"),
    scenario_attributes[["results"]],
    "Matrices",
    "time_aht.omx"
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

calculate_accessibility <- function(zone_centroids, facilities, status,
                                    scenario, travel_mode, travel_times,
                                    decay_time_min = 30) {
  zone_ids <- as.character(zone_centroids$zone)
  facility_zones <- unique(as.character(facilities$zone))
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
  unique_zone_travel_times[travel_time_indices] <- selected_travel_times

  if (anyNA(unique_zone_travel_times) || any(!is.finite(unique_zone_travel_times))) {
    rlang::abort(
      sprintf("%s travel times are missing for one or more zone pairs.", travel_mode)
    )
  }
  if (any(unique_zone_travel_times < 0)) {
    rlang::abort(sprintf("%s travel times cannot be negative.", travel_mode))
  }

  facility_indices <- match(as.character(facilities$zone), facility_zones)
  travel_time_matrix <- unique_zone_travel_times[, facility_indices, drop = FALSE]
  nearest_travel_time_min <- apply(travel_time_matrix, 1, min)
  accessibility_score <- rowSums(exp(-travel_time_matrix / decay_time_min))

  closest_accessibility_score <- exp(
    -nearest_travel_time_min / decay_time_min
  )

  zone_centroids %>%
    dplyr::mutate(
      health_center_accessibility_score = accessibility_score,
      health_center_nearest_travel_time_min = nearest_travel_time_min,
      health_center_closest_accessibility_score = closest_accessibility_score,
      facility_set = status,
      travel_time_scenario = scenario,
      travel_mode = travel_mode
    )
}

facility_sets <- list(
  current = list(
    facilities = health_centers %>% dplyr::filter(status == "Nykyinen"),
    scenario = "2023"
  ),
  all_2045_ve0 = list(
    facilities = health_centers,
    scenario = "2045_ve0"
  ),
  all_2045_suunnitelma = list(
    facilities = health_centers,
    scenario = "2045_suunnitelma"
  )
)

travel_modes <- c(
  car_work = "automatka-aika",
  transit_work = "joukkoliikenteen matka-aika"
)
travel_times_by_scenario <- facility_sets %>%
  purrr::map("scenario") %>%
  unlist() %>%
  unique() %>%
  purrr::set_names() %>%
  purrr::map(read_travel_times)

accessibility_results <- purrr::map_dfr(
  names(facility_sets),
  function(facility_set_name) {
    facility_set <- facility_sets[[facility_set_name]]
    purrr::map_dfr(
      names(travel_modes),
      function(travel_mode) {
        calculate_accessibility(
          zone_centroids,
          facility_set$facilities,
          facility_set_name,
          facility_set$scenario,
          travel_mode,
          travel_times_by_scenario[[facility_set$scenario]]
        )
      }
    )
  }
)

rescale_accessibility <- function(score) {
  score_range <- range(score, na.rm = TRUE)

  if (diff(score_range) == 0) {
    rep(100, length(score))
  } else {
    scales::rescale(score, to = c(0, 100))
  }
}

accessibility_results <- accessibility_results %>%
  dplyr::group_by(travel_mode) %>%
  dplyr::mutate(
    health_center_accessibility = rescale_accessibility(
      health_center_accessibility_score
    ),
    health_center_closest_accessibility = rescale_accessibility(
      health_center_closest_accessibility_score
    )
  ) %>%
  dplyr::ungroup()

readr::write_rds(
  accessibility_results,
  file = here::here("results", "health-centers-accessibility.rds")
)

# Maps --------------------------------------------------------------------

figure_directory <- here::here("figures", "health-centers")
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

plot_accessibility <- function(data, title, subtitle, accessibility_column) {
  ggplot() +
    geom_sf(
      mapping = aes(fill = .data[[accessibility_column]]),
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
  all_2045_ve0 = paste(
    "Terveyskeskusten saavutettavuus nykyisillä ja suunnitelluilla kohteilla",
    "(2045 vertailupohja)"
  ),
  all_2045_suunnitelma = paste(
    "Terveyskeskusten saavutettavuus nykyisillä ja suunnitelluilla kohteilla",
    "(2045 MAL-suunnitelma)"
  )
)

purrr::walk(names(travel_modes), function(travel_mode) {
  purrr::walk(
    names(facility_sets),
    function(facility_set) {
      data <- zone_polygons %>%
        dplyr::left_join(
          accessibility_results %>%
            dplyr::filter(
              .data$facility_set == .env$facility_set,
              .data$travel_mode == .env$travel_mode
            ) %>%
            sf::st_drop_geometry() %>%
            dplyr::select(
              zone,
              health_center_accessibility_score,
              health_center_accessibility,
              health_center_nearest_travel_time_min,
              health_center_closest_accessibility_score,
              health_center_closest_accessibility
            ),
          by = "zone"
        )

      plot <- plot_accessibility(
        data,
        title = map_titles[[facility_set]],
        subtitle = paste(
          "30 minuutin vaimenemalla painotettu saavutettavuus,",
          travel_modes[[travel_mode]]
        ),
        accessibility_column = "health_center_accessibility"
      )

      ggsave_map(
        here::here(
          "figures", "health-centers",
          sprintf(
            "map_health-centers-accessibility_%s_%s.png",
            facility_set,
            travel_mode
          )
        ),
        plot = plot
      )

      closest_plot <- plot_accessibility(
        data,
        title = paste(map_titles[[facility_set]], "(lähin kohde)"),
        subtitle = paste(
          "Saavutettavuus perustuu vain aikaan:",
          travel_modes[[travel_mode]],
          "lähimpään terveyskeskukseen"
        ),
        accessibility_column = "health_center_closest_accessibility"
      )

      ggsave_map(
        here::here(
          "figures", "health-centers",
          sprintf(
            "map_health-centers-accessibility-closest_%s_%s.png",
            facility_set,
            travel_mode
          )
        ),
        plot = closest_plot
      )
    }
  )
})

# MAL plan impact ---------------------------------------------------------

purrr::walk(names(travel_modes), function(travel_mode) {
  baseline_results <- accessibility_results %>%
    dplyr::filter(
      .data$facility_set == "all_2045_ve0",
      .data$travel_mode == .env$travel_mode
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      zone,
      baseline_accessibility = health_center_accessibility,
      baseline_closest_accessibility = health_center_closest_accessibility
    )
  plan_results <- accessibility_results %>%
    dplyr::filter(
      .data$facility_set == "all_2045_suunnitelma",
      .data$travel_mode == .env$travel_mode
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      zone,
      plan_accessibility = health_center_accessibility,
      plan_closest_accessibility = health_center_closest_accessibility
    )

  accessibility_difference <- zone_polygons %>%
    dplyr::left_join(baseline_results, by = "zone") %>%
    dplyr::left_join(plan_results, by = "zone") %>%
    dplyr::mutate(
      diff_accessibility = plan_accessibility - baseline_accessibility,
      diff_closest_accessibility = (
        plan_closest_accessibility - baseline_closest_accessibility
      )
    )

  plot_accessibility_difference <- function(data, column, title, subtitle) {
    difference_limit <- max(abs(data[[column]]), na.rm = TRUE)
    if (!is.finite(difference_limit) || difference_limit == 0) {
      difference_limit <- 1
    }

    ggplot() +
      geom_sf(
        mapping = aes(fill = .data[[column]]),
        data = data,
        color = "#D0CCC0",
        linewidth = 0.08
      ) +
      scale_fill_gradient2(
        low = "#7b1154",
        mid = "white",
        high = "#3E8606",
        midpoint = 0,
        limits = c(-difference_limit, difference_limit),
        name = "indeksin\nmuutos",
        labels = scales::label_number(accuracy = 1)
      ) +
      geom_basemap() +
      coord_sf_mal() +
      annotate_map(title = title, subtitle = subtitle) +
      theme_mal_map()
  }

  accessibility_difference_plot <- plot_accessibility_difference(
    accessibility_difference,
    column = "diff_accessibility",
    title = paste(
      "MAL-suunnitelman vaikutus terveyskeskusten saavutettavuuteen",
      sprintf("(%s)", travel_modes[[travel_mode]])
    ),
    subtitle = paste(
      "Saavutettavuusindeksin muutos:",
      "2045 MAL-suunnitelma - 2045 vertailupohja"
    )
  )
  ggsave_map(
    here::here(
      "figures", "health-centers",
      sprintf("map_health-centers-accessibility-diff-all_%s.png", travel_mode)
    ),
    plot = accessibility_difference_plot
  )

  closest_accessibility_difference_plot <- plot_accessibility_difference(
    accessibility_difference,
    column = "diff_closest_accessibility",
    title = paste(
      "MAL-suunnitelman vaikutus lähimmän terveyskeskuksen saavutettavuuteen",
      sprintf("(%s)", travel_modes[[travel_mode]])
    ),
    subtitle = paste(
      "Lähimmän terveyskeskuksen indeksi:",
      "2045 MAL-suunnitelma - 2045 vertailupohja"
    )
  )
  ggsave_map(
    here::here(
      "figures", "health-centers",
      sprintf(
        "map_health-centers-accessibility-diff-closest_%s.png",
        travel_mode
      )
    ),
    plot = closest_accessibility_difference_plot
  )
})