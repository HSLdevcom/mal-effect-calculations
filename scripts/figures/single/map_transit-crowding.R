# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

# The toolbox export is saved before the road-only filters in links.R.
# links_<scenario>.rds excludes rail (types 2:6), including train and metro.
results <- sf::read_sf(here::here("results", sprintf("links_toolbox_%s.gpkg", scenario_attributes[["scenario"]]))) %>%
  dplyr::filter(area != "peripheral")
volume_factors <- readr::read_tsv(here::here("utilities", "volume_factors.tsv"), col_types = "cddd")

capacity_columns <- c(
  "transit_cap_total_aht", "transit_cap_seated_aht",
  "transit_cap_total_pt", "transit_cap_seated_pt",
  "transit_cap_total_iht", "transit_cap_seated_iht"
)
demand_columns <- c(
  "transit_leisure_aht", "transit_leisure_pt", "transit_leisure_iht", "transit_leisure_vrk",
  "transit_work_aht", "transit_work_pt", "transit_work_iht", "transit_work_vrk"
)
missing_demand_columns <- setdiff(demand_columns, names(results))
if (length(missing_demand_columns) > 0) {
  rlang::abort(sprintf("Missing transit demand columns: %s", paste(missing_demand_columns, collapse = ", ")))
}

missing_capacity_columns <- setdiff(capacity_columns, names(results))
if (length(missing_capacity_columns) > 0) {
  warning(sprintf(
    "Skipping transit crowding maps for scenario %s: missing capacity columns (%s).",
    scenario_attributes[["scenario"]],
    paste(missing_capacity_columns, collapse = ", ")
  ))
}

if (length(missing_capacity_columns) == 0) {
  work_demand_vrk <- sum(results$transit_work_vrk, na.rm = TRUE)
  leisure_demand_vrk <- sum(results$transit_leisure_vrk, na.rm = TRUE)
  total_demand_vrk <- work_demand_vrk + leisure_demand_vrk
  if (!is.finite(total_demand_vrk) || total_demand_vrk <= 0) {
    rlang::abort("Cannot estimate VRK capacity without positive transit VRK demand.")
  }

  work_share <- work_demand_vrk / total_demand_vrk
  leisure_share <- leisure_demand_vrk / total_demand_vrk
  transit_factors <- volume_factors %>%
    dplyr::filter(mode %in% c("transit_work", "transit_leisure")) %>%
    dplyr::select(mode, aht, pt, iht) %>%
    tidyr::pivot_longer(-mode, names_to = "period", values_to = "share") %>%
    tidyr::pivot_wider(names_from = mode, values_from = share) %>%
    dplyr::mutate(
      expansion = 1 / (work_share * transit_work + leisure_share * transit_leisure)
    )

  periods <- tibble::tribble(
    ~period, ~period_name,       ~minimum_demand,
    "aht",  "aamuhuipputunti",  20,
    "pt",   "päivä",             20,
    "iht",  "iltahuipputunti",   20,
    "vrk",  "vuorokausi",       200
  )
  capacity_types <- tibble::tribble(
    ~capacity_field, ~capacity_name,
    "total", "kokonaiskapasiteetti",
    "seated", "istumapaikkakapasiteetti"
  )

  crowding_maps <- tidyr::crossing(periods, capacity_types) %>%
    dplyr::mutate(
      demand = purrr::map(period, ~ results[[paste0("transit_leisure_", .x)]] +
                            results[[paste0("transit_work_", .x)]]),
      capacity_values = purrr::map2(period, capacity_field, function(period, capacity_field) {
        if (period == "vrk") {
          period_expansions <- transit_factors$expansion[match(c("aht", "pt", "iht"), transit_factors$period)]
          Reduce(`+`, Map(function(period, expansion) {
            results[[paste0("transit_cap_", capacity_field, "_", period)]] * expansion
          }, c("aht", "pt", "iht"), period_expansions))
        } else {
          results[[paste0("transit_cap_", capacity_field, "_", period)]]
        }
      }),
      label = sprintf("Joukkoliikenteen ruuhkaisuus - %s (%s)", period_name, capacity_name),
      output = sprintf("map_transit-crowding_%s_%s_%s.png",
                       period, capacity_field, scenario_attributes[["scenario"]])
    ) %>%
    dplyr::mutate(
      data = purrr::pmap(
        list(demand, capacity_values, minimum_demand),
        function(demand, capacity_values, minimum_demand) {
        results %>%
          dplyr::mutate(crowding = dplyr::if_else(
            demand >= minimum_demand & capacity_values > 0,
            demand / capacity_values,
            NA_real_
          ),
          buffer_width = dplyr::case_when(
            capacity_values >= 1000 ~ 100,
            capacity_values >= 200 ~ 50,
            TRUE ~ 30
          )) %>%
          dplyr::filter(!is.na(crowding)) %>%
          # Buffer to the right of each directed link, as in the volume maps.
          sf::st_buffer(dist = -.$buffer_width,
                        endCapStyle = "FLAT",
                        singleSide = TRUE)
      })
    )


# Plot --------------------------------------------------------------------

  purrr::pwalk(crowding_maps, function(period, period_name, capacity_field, capacity_name,
                                      minimum_demand, demand, capacity_values, label, output, data) {
    plot <- ggplot() +
      geom_basemap(show_roads = FALSE) +
      geom_sf(mapping = aes(fill = crowding),
              data = data, color = NA) +
      scale_fill_stepsn(
        colours = c("#7fde40", "#ffff08", "#ffa94c"),
        values = c(0.1, 0.5, 0.9),
        labels = scales::label_percent(accuracy = 1, suffix = " %"),
        name = NULL,
        breaks = seq(0.2, 1, by = 0.2),
        limits = c(0, 1),
        oob = scales::squish
      ) +
      coord_sf_mal() +
      annotate_map(
        title = label,
        subtitle = sprintf("%d %s", scenario_attributes[["year"]], scenario_attributes[["name"]])
      ) +
      theme_mal_map()

    print(plot)
    ggsave_map(here::here("figures", output))
  })
}
