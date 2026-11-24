# OD values are validated by read_scenario_transfers() before aggregation.
summarise_transfer_origins <- function(od_transfers, regional_zone_ids) {
  od_transfers %>%
    dplyr::mutate(
      boardings = dplyr::if_else(
        transit_demand > 0, transit_demand * average_boardings, 0
      ),
      transfer_events = dplyr::if_else(
        transit_demand > 0, transit_demand * pmax(average_boardings - 1, 0), 0
      ),
      within_region = origin %in% regional_zone_ids & destination %in% regional_zone_ids
    ) %>%
    dplyr::group_by(origin) %>%
    dplyr::summarise(
      regional_transit_demand = sum(transit_demand[within_region]),
      regional_boardings = sum(boardings[within_region]),
      regional_transfer_events = sum(transfer_events[within_region]),
      transit_demand = sum(transit_demand),
      transfer_events = sum(transfer_events),
      .groups = "drop"
    )
}

# Origin aggregates retain separate counts for OD pairs within the region.
# Group ratios must be calculated from summed boardings and demand, rather
# than averaging zone ratios or adding 1 to transfers/trips (zero-boarding
# intrazonal trips need not have an initial boarding).
summarise_transfer_areas <- function(origin_totals, zone_areas) {
  if (anyDuplicated(zone_areas$zone) || anyNA(zone_areas$area)) {
    stop("Zone IDs must be unique and municipal groups must be defined.")
  }
  if (anyDuplicated(origin_totals[c("scenario", "origin")])) {
    stop("Origin totals must have one row per scenario and origin.")
  }
  for (scenario_id in unique(origin_totals$scenario)) {
    missing_zones <- setdiff(
      zone_areas$zone, origin_totals$origin[origin_totals$scenario == scenario_id]
    )
    if (length(missing_zones) > 0) stop("Regional origin totals are missing.")
  }

  grouped <- origin_totals %>%
    dplyr::inner_join(zone_areas, by = c("origin" = "zone")) %>%
    dplyr::group_by(scenario, area) %>%
    dplyr::summarise(
      transit_demand = sum(regional_transit_demand),
      boardings = sum(regional_boardings),
      transfer_events = sum(regional_transfer_events),
      .groups = "drop"
    )
  regional <- grouped %>%
    dplyr::group_by(scenario) %>%
    dplyr::summarise(
      dplyr::across(c(transit_demand, boardings, transfer_events), sum),
      .groups = "drop"
    ) %>%
    dplyr::mutate(area = "helsinki_region")

  dplyr::bind_rows(grouped, regional) %>%
    dplyr::mutate(boardings_per_trip = dplyr::if_else(
      transit_demand > 0, boardings / transit_demand, NA_real_
    ))
}
