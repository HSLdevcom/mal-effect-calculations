# -*- coding: utf-8-unix -*-
# Export individual sensitivity and main scenarios after sensitivity.R in run.R.
# Install the Excel writer once with install.packages("writexl").
library(here)
library(tidyverse)

local({
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop('Excel export requires writexl. Run install.packages("writexl") and source this script again.')
  }

  # Keep scenario IDs: read_and_bind() uses display names that are shared by
  # main scenarios and their sensitivity variants.
  scenario_ids <- unique(c(scenario_list, sensitivity_scenario_list))
  metadata <- tibble::tibble(scenario = scenario_ids) %>%
    dplyr::left_join(
      dplyr::select(scenarios, scenario, year, name, sensitivity, root),
      by = "scenario"
    )

  if (anyNA(metadata$root) || !all(metadata$root %in% scenario_ids)) {
    stop("Every exported scenario and its root must be included in the scenario lists and scenarios.tsv.")
  }

  # Archived inputs describe the run even if the forecast inputs later change.
  # Older runs have no archive, so use their configured forecast directory.
  find_cost_file <- function(id, extension) {
    attributes <- scenarios[match(id, scenarios$scenario), ]
    directories <- c(
      file.path(config::get("helmet_data"), attributes$results, "inputs", attributes$forecast),
      file.path(config::get("forecast_zonedata_path"), attributes$forecast)
    )
    for (directory in directories) {
      files <- list.files(directory, pattern = paste0("\\.", extension, "$"),
                          full.names = TRUE, ignore.case = TRUE)
      if (length(files) > 1) {
        stop(sprintf("Multiple .%s files for scenario '%s' in %s.", extension, id, directory))
      }
      if (length(files) == 1) return(files)
    }
    stop(sprintf("No .%s file found for scenario '%s' in: %s",
                 extension, id, paste(directories, collapse = ", ")))
  }

  cost_inputs <- purrr::map_dfr(scenario_ids, function(id) {
    cco_file <- find_cost_file(id, "cco")
    tco_file <- find_cost_file(id, "tco")
    cco <- readr::read_tsv(cco_file, comment = "#", col_types = "d",
                            show_col_types = FALSE)
    if (!"dist_cost" %in% names(cco) || nrow(cco) != 1 ||
        !is.finite(cco$dist_cost) || cco$dist_cost < 0) {
      stop(sprintf("Expected one non-negative dist_cost value in %s.", cco_file))
    }

    tco_name <- tolower(basename(tco_file))
    percent <- stringr::str_match(tco_name, "_([0-9]+(?:[.,][0-9]+)?)pct\\.tco$")[, 2]
    tco_factor <- if (!is.na(percent)) {
      as.numeric(sub(",", ".", percent, fixed = TRUE)) / 100
    } else if (grepl("^[0-9]{4}\\.tco$", tco_name)) {
      1
    } else {
      stop(sprintf("Cannot determine TCO factor from filename: %s.", tco_file))
    }

    tibble::tibble(scenario = id, cco_eur_per_km = cco$dist_cost,
                   tco_factor = tco_factor)
  })
  metadata <- dplyr::left_join(metadata, cost_inputs, by = "scenario")

  read_scenario_results <- function(prefix, columns) {
    purrr::map_dfr(scenario_ids, function(id) {
      path <- here::here("results", sprintf("%s_%s.rds", prefix, id))
      if (!file.exists(path)) {
        stop(sprintf("Missing scenario results: %s. Run the preparation steps in run.R first.", path))
      }
      readr::read_rds(path) %>%
        dplyr::select(dplyr::all_of(columns)) %>%
        dplyr::mutate(scenario = id, .before = 1)
    })
  }

  # Shares are the existing origin-based tour shares, including park and ride.
  modes <- read_scenario_results("areas", c("area", "origin_share_sustainable")) %>%
    dplyr::transmute(scenario, area = as.character(area),
                     sustainable_share_percent = 100 * origin_share_sustainable)

  root_modes <- modes %>%
    dplyr::rename(root = scenario, root_share_percent = sustainable_share_percent)

  modes <- metadata %>%
    dplyr::left_join(modes, by = "scenario") %>%
    dplyr::left_join(root_modes, by = c("root", "area")) %>%
    dplyr::mutate(change_vs_root_pp = sustainable_share_percent - root_share_percent)

  # emissions.R stores corrected annual CO2-equivalent emissions in grams.
  # Keep its precomputed total row rather than summing it with vehicle rows.
  emissions <- read_scenario_results("emissions", c("vehicle", "emission")) %>%
    dplyr::transmute(scenario, vehicle = as.character(vehicle),
                     emissions_tonnes_per_year = emission / 10^6)

  root_emissions <- emissions %>%
    dplyr::rename(root = scenario, root_emissions_tonnes_per_year = emissions_tonnes_per_year)

  emissions <- metadata %>%
    dplyr::left_join(emissions, by = "scenario") %>%
    dplyr::left_join(root_emissions, by = c("root", "vehicle")) %>%
    dplyr::mutate(
      change_vs_root_tonnes_per_year = emissions_tonnes_per_year - root_emissions_tonnes_per_year,
      change_vs_root_percent = dplyr::if_else(
        root_emissions_tonnes_per_year == 0, NA_real_,
        100 * change_vs_root_tonnes_per_year / root_emissions_tonnes_per_year
      )
    )

  region <- "Helsingin seutu"
  summary_modes <- modes %>%
    dplyr::filter(area == region) %>%
    dplyr::select(scenario, sustainable_share_percent, change_vs_root_pp)
  summary_emissions <- emissions %>%
    dplyr::filter(vehicle == "total") %>%
    dplyr::select(scenario, emissions_tonnes_per_year,
                   change_vs_root_tonnes_per_year, change_vs_root_percent)

  if (nrow(summary_modes) != length(scenario_ids) ||
      nrow(summary_emissions) != length(scenario_ids) ||
      anyDuplicated(summary_modes$scenario) || anyDuplicated(summary_emissions$scenario)) {
    stop("Expected one Helsinki region mode share and one total emissions row per scenario.")
  }

  summary <- metadata %>%
    dplyr::left_join(summary_modes, by = "scenario") %>%
    dplyr::left_join(summary_emissions, by = "scenario")

  notes <- tibble::tribble(
    ~item, ~description,
    "Scenarios", "Includes scenario_list and sensitivity_scenario_list from run.R, in that order.",
    "Scenario identity", "scenario is the unique ID from scenarios.tsv; name can be shared by multiple scenarios.",
    "CCO", "cco_eur_per_km is dist_cost read from the scenario's .cco file, in EUR/km.",
    "TCO", "tco_factor comes from the .tco filename: _120pct means 1.2, _80pct means 0.8, and a plain year filename (such as 2026.tco) means 1.0.",
    "Cost input sources", "Prefer the result folder's archived inputs; fall back to the configured forecast directory when an archived file is unavailable.",
    "Summary", "Sustainable share and total annual road-traffic emissions for Helsingin seutu (Helsinki region).",
    "Sustainable_modes", "All areas from areas_<scenario>.rds; areas overlap and should not be added together.",
    "Sustainable share", "Percentage of tours starting in the area: transit, bike, walk and park and ride. Values run from 0 to 100.",
    "Emissions", "Corrected annual CO2-equivalent emissions from emissions_<scenario>.rds, converted from grams to tonnes (divide by 1,000,000).",
    "Vehicle total", "The total vehicle row already includes the individual vehicle categories; do not sum it with those categories.",
    "Root comparisons", "Changes compare each scenario to its root in scenarios.tsv, for the same area or vehicle. Main scenarios compare to themselves.",
    "Change units", "pp means percentage points; emissions changes use tonnes/year or percent (0 to 100 scale). Positive values mean an increase.",
    "Missing values", "Missing results and percentage changes against zero root emissions are left blank.",
    "Sources", "Individual scenario RDS files retain sensitivity variants; *_all.rds files retain only main scenarios and sensitivity bounds."
  )

  output <- here::here("results", "sensitivity_results.xlsx")
  writexl::write_xlsx(
    list(Summary = summary, Sustainable_modes = modes, Emissions = emissions, Notes = notes),
    path = output
  )
  message(sprintf("Sensitivity comparison workbook written to %s", output))
})
