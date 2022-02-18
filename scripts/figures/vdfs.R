# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Read data ---------------------------------------------------------------

translations <- here::here("utilities", "vdfs.tsv") %>%
  readr::read_tsv(col_types = "ic")

vdfs <- read_tsv_helmet(
  here::here(config::get("helmet_data"), scenario_attributes[["results"]], "vehicle_kms_vdfs_areas.txt"),
  col_types = "cddddddd",
  first_col_name = "area"
)

# Discard peripheral data, transpose data frame, and calculate total vehicle
# kilometers.
vdfs <- vdfs %>%
  dplyr::filter(!area %in% "peripheral") %>%
  tidyr::pivot_longer(-area, names_to = "vdf", values_to = "vehicle_kms") %>%
  tidyr::pivot_wider(id_cols = vdf, names_from = area, values_from = vehicle_kms) %>%
  dplyr::mutate(total = rowSums(select(., -vdf)))

# Handle vdf names.
vdfs <- vdfs %>%
  dplyr::mutate(
    vdf = factor(vdf, levels = translations$level, labels = translations$label),
    vdf = forcats::fct_collapse(
      vdf,
      `Muut` = c("Muut", "Apulinkit"),
      `Kokooja- ja tonttikadut` = c("Kokooja- ja tonttikadut"),
      `Pääkadut` = c(
        "Pääkadut",
        "Useampikaistaiset pääkadut tasoliittymin valoilla"
      ),
      `Pääväylät eritasoliittymin, maantiet` = c(
        "Maantiet / Useampikaistaiset kaupunkiväylät eritasoliittymin",
        "Moottoritiet"
      )
    ),
    vdf = forcats::fct_rev(vdf)
  )

# Calculate results.
vdfs <- vdfs %>%
  dplyr::group_by(vdf) %>%
  dplyr::summarise(total = sum(total), .groups = "drop") %>%
  dplyr::mutate(share = total / sum(total))


# Output ------------------------------------------------------------------

readr::write_rds(vdfs, file = here::here("results", sprintf("vdfs_%s.rds", scenario_attributes[["scenario"]])))
