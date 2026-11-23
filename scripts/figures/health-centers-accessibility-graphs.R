# -*- coding: utf-8-unix -*-
# Can also be sourced independently after health-centers-accessibility.R.
library(here)
library(tidyverse)
library(sf)

source(here::here("scripts", "figures", "validate_scenarios.R"), encoding = "utf-8")
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

local({
  accessibility <- readr::read_rds(
    here::here("results", "health-centers-accessibility.rds")
  ) %>%
    sf::st_drop_geometry()

  cached_scenario_ids <- unique(accessibility$travel_time_scenario)
  if (any(!cached_scenario_ids %in% scenarios$scenario)) {
    rlang::abort("Accessibility results contain scenarios missing from scenarios.tsv.")
  }
  scenario_ids <- scenarios$scenario[!scenarios$sensitivity]
  scenario_definitions <- scenarios %>%
    dplyr::filter(
      .data$scenario %in% scenario_ids |
        (.data$sensitivity & .data$root %in% scenario_ids)
    )
  if (any(!scenario_definitions$scenario %in% cached_scenario_ids)) {
    rlang::abort(paste(
      "Main or sensitivity travel times are missing from the accessibility cache.",
      "Rerun scripts/figures/health-centers-accessibility.R first."
    ))
  }
  accessibility <- accessibility %>%
    dplyr::filter(.data$travel_time_scenario %in% scenario_definitions$scenario)
  scenario_labels <- vapply(scenario_ids, function(scenario) {
    attributes <- scenarios[match(scenario, scenarios$scenario), ]
    sprintf("%d %s", attributes[["year"]], attributes[["name"]])
  }, character(1))

  area_ids <- c(
    "helsinki_cbd", "helsinki_other", "espoo_vant_kau",
    "surround_train", "surround_other"
  )
  translations <- readr::read_tsv(
    here::here("utilities", "areas.tsv"), col_types = "cc"
  )
  area_labels <- translations$label[match(area_ids, translations$level)]

  # The zone caches contain each scenario's own population and the common
  # municipality-group classification used by the other regional charts.
  population <- purrr::map_dfr(scenario_definitions$scenario, function(scenario) {
    readr::read_rds(here::here(
      "results", sprintf("zones_%s.rds", scenario)
    )) %>%
      sf::st_drop_geometry() %>%
      dplyr::transmute(
        zone = as.character(zone), area, total_pop,
        travel_time_scenario = scenario
      ) %>%
      dplyr::filter(.data$area %in% area_ids)
  })

  if (anyDuplicated(population[c("zone", "travel_time_scenario")]) > 0 ||
      any(!is.finite(population$total_pop)) || any(population$total_pop < 0)) {
    rlang::abort("Expected unique zones with finite, non-negative populations.")
  }

  # Keep population zones on the left so missing travel times cannot silently
  # remove residents from the weighted average.
  weighted_times <- purrr::map_dfr(unique(accessibility$facility_group), function(group) {
    purrr::map_dfr(unique(accessibility$travel_mode), function(mode) {
      times <- accessibility %>%
        dplyr::filter(.data$facility_group == group, .data$travel_mode == mode) %>%
        dplyr::select(zone, travel_time_scenario, closest_travel_time_min)
      if (anyDuplicated(times[c("zone", "travel_time_scenario")]) > 0) {
        rlang::abort("Expected one closest-facility travel time per scenario and zone.")
      }
      population %>%
        dplyr::left_join(times, by = c("zone", "travel_time_scenario")) %>%
        dplyr::mutate(facility_group = group, travel_mode = mode)
    })
  })

  if (any(!is.finite(weighted_times$closest_travel_time_min)) ||
      any(weighted_times$closest_travel_time_min < 0)) {
    rlang::abort("Expected finite, non-negative travel times for every population zone.")
  }

  results <- weighted_times %>%
    dplyr::group_by(facility_group, travel_mode, travel_time_scenario, area) %>%
    dplyr::summarise(
      mean_travel_time_min = stats::weighted.mean(closest_travel_time_min, total_pop),
      total_pop = sum(total_pop),
      .groups = "drop"
    )

  if (any(results$total_pop <= 0) || any(!is.finite(results$mean_travel_time_min))) {
    rlang::abort("Each municipality group must have a positive total population.")
  }

  # Aggregate each sensitivity using its own population before taking the
  # range. Include the main scenario so its bar is always inside the interval.
  results <- results %>%
    dplyr::left_join(
      scenario_definitions %>%
        dplyr::select(travel_time_scenario = scenario, root, present, sensitivity),
      by = "travel_time_scenario"
    )
  limits <- results %>%
    dplyr::filter(!present) %>%
    dplyr::group_by(facility_group, travel_mode, area, root) %>%
    dplyr::summarise(
      mean_travel_time_min_lower = min(mean_travel_time_min),
      mean_travel_time_min_upper = max(mean_travel_time_min),
      .groups = "drop"
    )
  readr::write_csv(results, here::here(
    "results", "health-centers-accessibility-areas-all.csv"
  ))
  results <- results %>%
    dplyr::filter(!sensitivity) %>%
    dplyr::left_join(limits, by = c("facility_group", "travel_mode", "area", "root"))

  readr::write_csv(results, here::here(
    "results", "health-centers-accessibility-areas.csv"
  ))
  results <- results %>%
    dplyr::mutate(
      area = factor(area, levels = area_ids, labels = area_labels),
      scenario = factor(
        travel_time_scenario, levels = scenario_ids, labels = scenario_labels
      )
    )

  destinations <- c(
    hospitals = "lähimpään sairaalaan",
    other_health = "lähimpään terveys- tai hyvinvointikeskukseen"
  )
  modes <- c(car_leisure = "henkilöauto", transit_leisure = "joukkoliikenne")
  figure_directory <- here::here("figures", "health-centers")
  dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

  for (group in unique(results$facility_group)) {
    for (mode in unique(results$travel_mode)) {
      data <- results %>%
        dplyr::filter(.data$facility_group == group, .data$travel_mode == mode)
      plot <- ggplot(data, aes(x = area, y = mean_travel_time_min, fill = scenario)) +
        geom_col(position = position_dodge2()) +
        geom_errorbar(
          aes(ymin = mean_travel_time_min_lower, ymax = mean_travel_time_min_upper),
          position = position_dodge2(width = 0.9, padding = 0.66),
          color = "#333333", linewidth = 0.35, na.rm = TRUE
        ) +
        geom_text(
          aes(
            y = mean_travel_time_min / 2,
            label = scales::label_number(accuracy = 0.1, decimal.mark = ",")(
              mean_travel_time_min
            ),
            color = scenario
          ),
          position = position_dodge2(width = 0.9),
          size = points2mm(8)
        ) +
        scale_y_continuous(
          labels = scales::label_number(decimal.mark = ","),
          expand = expansion(mult = c(0.025, 0.1))
        ) +
        scale_x_discrete(labels = scales::label_wrap(5), drop = FALSE) +
        scale_fill_manual(name = NULL, values = mal_greens_fill) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
        scale_color_manual(guide = "none", values = mal_color) +
        labs(
          title = stringr::str_wrap(paste(
            "Keskimääräinen matka-aika", destinations[[group]],
            "asukkailla painotettuna", sprintf("(%s)", modes[[mode]])
          ), width = 65),
          x = NULL, y = "minuuttia"
        ) +
        theme_mal_graph()

      ggsave_graph(
        file.path(figure_directory, sprintf(
          "graph_health-centers-accessibility_%s_%s.png", group, mode
        )),
        plot = plot, width = 190, height = 100
      )
    }
  }
})
