# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

# Generate this file with scripts/create_centers_and_stations.R whenever
# the scenario networks or center coverage change.
centers_and_stations <- sf::read_sf(here::here("data", "centers_and_stations.gpkg"))

readr::write_rds(centers_and_stations, file = here::here("results", "centers-and-stations.rds"))


# Plot --------------------------------------------------------------------

coverage_scenarios <- c(
  "2023" = "2023 Nykytila",
  "2045_vertailupohja" = "2045 Vertailupohja",
  "2045_tavoiteverkko" = "2045 Tavoiteverkko",
  "2045_suunnitelma" = "2045 Suunnitelma"
)

for (coverage_scenario in names(coverage_scenarios)) {
  station_column <- paste0("station_", coverage_scenario)
  scenario_coverage <- centers_and_stations %>%
    dplyr::filter(center | .data[[station_column]]) %>%
    # Generated station and center polygons overlap. Draw centers last so
    # station buffers do not hide their fill.
    dplyr::arrange(center)

  coverage_map <- ggplot() +
    geom_sf(mapping = aes(fill = center),
            data = scenario_coverage, color = NA) +
    geom_basemap(scenario_ids = coverage_scenario) +
    scale_fill_manual("Alue",
                      values = c("FALSE" = "#BFD7AC", "TRUE" = "#3E8606"),
                      labels = c("Raskas raideliikenne", "Seudun keskus")) +
    coord_sf_mal() +
    annotate_map(
      title = "Seudun keskukset ja raskas raideliikenne",
      subtitle = coverage_scenarios[[coverage_scenario]]
    ) +
    theme_mal_map()

  ggsave_map(
    here::here("figures", paste0("map_basemap_centers-and-stations_",
                                 coverage_scenario, ".png")),
    plot = coverage_map
  )
}
