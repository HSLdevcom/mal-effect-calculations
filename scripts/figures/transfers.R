# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)

source(here::here("scripts", "figures", "validate_scenarios.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

# Data --------------------------------------------------------------------

scenario_names <- c("2023","2045_ve0", "2045_suunnitelma")
transfer_matrix_name <- "transit_transit_work_avg_boardings"

read_scenario_transfers <- function(scenario) {
  scenario_attributes <- set_scenario(scenario)
  matrices_directory <- file.path(
    config::get("helmet_data"),
    scenario_attributes[["results"]],
    "Matrices"
  )
  boardings <- read_helmet_omx(
    file.path(matrices_directory, "time_aht.omx"),
    matrix_name = transfer_matrix_name
  )
  demand <- read_helmet_omx(
    file.path(matrices_directory, "demand_aht.omx"),
    matrix_name = "transit_work"
  )

  if (!transfer_matrix_name %in% names(boardings)) {
    rlang::abort(
      sprintf(
        "Matrix '%s' is missing from %s.",
        transfer_matrix_name,
        file.path(matrices_directory, "time_aht.omx")
      )
    )
  }
  if (!"transit_work" %in% names(demand)) {
    rlang::abort(
      sprintf(
        "Matrix 'transit_work' is missing from %s.",
        file.path(matrices_directory, "demand_aht.omx")
      )
    )
  }

  boardings <- boardings %>%
    dplyr::select(
      origin,
      destination,
      average_boardings = dplyr::all_of(transfer_matrix_name)
    )
  demand <- demand %>%
    dplyr::select(origin, destination, transit_demand = transit_work)

  if (anyDuplicated(boardings[c("origin", "destination")]) ||
      anyDuplicated(demand[c("origin", "destination")])) {
    rlang::abort(sprintf("OD pairs are duplicated in scenario %s.", scenario))
  }

  od_transfers <- demand %>%
    dplyr::left_join(boardings, by = c("origin", "destination"))

  if (anyNA(od_transfers$transit_demand) ||
      any(!is.finite(od_transfers$transit_demand)) ||
      any(od_transfers$transit_demand < 0)) {
    rlang::abort(sprintf("Invalid transit_work demand in scenario %s.", scenario))
  }

  positive_demand <- od_transfers$transit_demand > 0
  single_precision_tolerance <- 2^-22
  invalid_average_boardings <-
    is.na(od_transfers$average_boardings) |
    !is.finite(od_transfers$average_boardings) |
    (
      od_transfers$average_boardings < 1 - single_precision_tolerance &
        !(od_transfers$average_boardings == 0 &
            od_transfers$origin == od_transfers$destination)
    )
  if (any(invalid_average_boardings[positive_demand])) {
    problematic_pair <- which(positive_demand & invalid_average_boardings)[1]
    rlang::abort(
      sprintf(
        paste(
          "Invalid average boardings for transit demand in scenario %s:",
          "value %s for origin %s and destination %s."
        ),
        scenario,
        od_transfers$average_boardings[problematic_pair],
        od_transfers$origin[problematic_pair],
        od_transfers$destination[problematic_pair]
      )
    )
  }

  origin_totals <- od_transfers %>%
    dplyr::mutate(
      transfer_events = dplyr::if_else(
        transit_demand > 0,
        transit_demand * pmax(average_boardings - 1, 0),
        0
      )
    ) %>%
    dplyr::group_by(origin) %>%
    dplyr::summarise(
      transit_demand = sum(transit_demand),
      transfer_events = sum(transfer_events),
      .groups = "drop"
    ) %>%
    dplyr::mutate(scenario = scenario)

  if (any(!is.finite(origin_totals$transfer_events))) {
    rlang::abort(sprintf("Invalid transfer totals in scenario %s.", scenario))
  }

  origin_totals
}

transfers_by_scenario <- purrr::map_dfr(
  scenario_names,
  read_scenario_transfers
)

zone_polygons <- readr::read_rds(
  here::here("results", "zones_2045_ve0.rds")
) %>%
  dplyr::mutate(zone = as.character(zone)) %>%
  dplyr::select(zone, geometry)

if (anyDuplicated(zone_polygons$zone)) {
  rlang::abort("The 2045 baseline zone geometry contains duplicate zone IDs.")
}

transfers_by_scenario <- transfers_by_scenario %>%
  dplyr::mutate(origin = as.character(origin))

missing_zone_origins <- setdiff(
  zone_polygons$zone,
  unique(transfers_by_scenario$origin)
)
if (length(missing_zone_origins) > 0) {
  rlang::abort(
    sprintf(
      "Transit OD matrix origins are missing for geometry zones: %s.",
      paste(head(missing_zone_origins, 10), collapse = ", ")
    )
  )
}

scenario_labels <- c(
  "2023" = "2023 nykytilanne",
  "2045_ve0" = "2045 vertailupohja",
  "2045_suunnitelma" = "2045 MAL-suunnitelma"
)
scenario_order <- c("2023", "2045_ve0", "2045_suunnitelma")
scenario_label_levels <- unname(scenario_labels[scenario_order])

zone_results <- zone_polygons %>%
  dplyr::left_join(
    transfers_by_scenario %>%
      dplyr::mutate(
        scenario_label = factor(
          scenario_labels[scenario],
          levels = scenario_label_levels
        )
      ),
    by = c("zone" = "origin")
  )

if (anyNA(zone_results$transfer_events) ||
    anyNA(zone_results$transit_demand)) {
  rlang::abort("Transit demand or transfer totals are missing for one or more zones.")
}

scenario_totals <- transfers_by_scenario %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(
    transit_demand = sum(transit_demand),
    transfer_events = sum(transfer_events),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    scenario_label = factor(scenario_labels[scenario], levels = scenario_label_levels)
  )

# Graph -------------------------------------------------------------------

transfer_figures_directory <- here::here("figures", "transfers")
if (!dir.exists(transfer_figures_directory) &&
    !dir.create(transfer_figures_directory, recursive = TRUE)) {
  rlang::abort(sprintf("Could not create output directory %s.", transfer_figures_directory))
}

graph_data <- scenario_totals %>%
  tidyr::pivot_longer(
    cols = c(transit_demand, transfer_events),
    names_to = "measure",
    values_to = "amount"
  ) %>%
  dplyr::mutate(
    measure = factor(
      measure,
      levels = c("transit_demand", "transfer_events"),
      labels = c("Joukkoliikennematkat", "Arvioidut vaihtotapahtumat")
    )
  )

ggplot(graph_data, aes(x = measure, y = amount, fill = scenario_label)) +
  geom_col(position = position_dodge2()) +
  geom_text(
    aes(
      y = amount / 2,
      label = scales::label_number(
        scale = 1e-3,
        accuracy = 1,
        decimal.mark = ","
      )(amount),
      color = scenario_label
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      scale = 1e-3,
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_fill_manual(
    name = NULL,
    values = mal_greens_fill
  ) +
  scale_color_manual(
    guide = "none",
    values = mal_color
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(25)
  ) +
  labs(
    title = "Joukkoliikennematkat ja arvioidut vaihtotapahtumat",
    subtitle = paste(
      "Vaihtotapahtumat estimoitu keskimääräisistä nousuista",
      "painottamalla transit_work-kysynnällä"
    ),
    x = NULL,
    y = "Tuhatta matkaa / vaihtotapahtumaa"
  ) +
  theme_mal_graph()

ggsave_graph(
  file.path(transfer_figures_directory, "graph_transfers_2045.png"),
  width = 180,
  height = 100
)

# Maps --------------------------------------------------------------------

# MAL plan impact ---------------------------------------------------------

transfer_difference <- zone_polygons %>%
  dplyr::left_join(
    zone_results %>%
      sf::st_drop_geometry() %>%
      dplyr::select(zone, scenario, transfer_events) %>%
      tidyr::pivot_wider(
        names_from = scenario,
        values_from = transfer_events
      ) %>%
      dplyr::mutate(
        diff_transfer_events = (
          .data[["2045_suunnitelma"]] - .data[["2045_ve0"]]
        ),
        relative_diff_transfer_events = dplyr::if_else(
          .data[["2045_ve0"]] > 0,
          diff_transfer_events / .data[["2045_ve0"]],
          NA_real_
        )
      ) %>%
      dplyr::select(zone, diff_transfer_events, relative_diff_transfer_events),
    by = "zone"
  )

if (anyNA(transfer_difference$diff_transfer_events)) {
  rlang::abort("Transfer impact could not be calculated for one or more zones.")
}

difference_limit <- max(abs(transfer_difference$diff_transfer_events))
if (!is.finite(difference_limit) || difference_limit == 0) {
  difference_limit <- 1
}

transfer_difference_plot <- ggplot() +
  geom_sf(
    mapping = aes(fill = diff_transfer_events),
    data = transfer_difference,
    color = "#D0CCC0",
    linewidth = 0.08
  ) +
  scale_fill_gradient2(
    low = "#3E8606",
    mid = "white",
    high = "#7b1154",
    midpoint = 0,
    limits = c(-difference_limit, difference_limit),
    name = "Vaihtotapahtumien\nmuutos",
    labels = scales::label_number(
      accuracy = 1,
      big.mark = " ",
      decimal.mark = ","
    ),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "MAL-suunnitelman vaikutus joukkoliikenteen vaihtotapahtumiin",
    subtitle = paste(
      "Arvioitujen vaihtotapahtumien muutos alueelta alkavissa matkoissa:",
      "2045 MAL-suunnitelma - 2045 vertailupohja"
    )
  ) +
  theme_mal_map()

ggsave_map(
  file.path(
    transfer_figures_directory,
    "map_diff_transfers_2045_ve0-2045_suunnitelma.png"
  ),
  plot = transfer_difference_plot
)

relative_transfer_difference_plot <- ggplot() +
  geom_sf(
    mapping = aes(fill = relative_diff_transfer_events),
    data = transfer_difference,
    color = "#D0CCC0",
    linewidth = 0.08
  ) +
  scale_fill_gradient2(
    low = "#3E8606",
    mid = "white",
    high = "#7b1154",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.5),
    name = "Vaihtotapahtumien\nsuhteellinen muutos",
    labels = scales::label_percent(
      accuracy = 1,
      suffix = " %",
      decimal.mark = ","
    ),
    na.value = "#D9D9D9",
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "MAL-suunnitelman suhteellinen vaikutus vaihtotapahtumiin",
    subtitle = paste(
      "Muutos suhteessa 2045 vertailupohjaan;",
      "harmailla alueilla ei ollut vertailupohjassa vaihtoja"
    )
  ) +
  theme_mal_map()

ggsave_map(
  file.path(
    transfer_figures_directory,
    "map_diff_transfers_relative_2045_ve0-2045_suunnitelma.png"
  ),
  plot = relative_transfer_difference_plot
)
