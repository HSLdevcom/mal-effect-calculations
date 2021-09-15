# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

read_tsv_helmet <- function(..., comment = "#") {
  withCallingHandlers({
    readr::read_tsv(..., comment = comment) %>%
      dplyr::rename(area = X1) %>%
      dplyr::rename_with(~ gsub("-", "_", .x, fixed = TRUE))
  }, warning = function(w) {
    # Helmet results never include first column name
    if (conditionMessage(w) == "Missing column names filled in: 'X1' [1]") {
      invokeRestart("muffleWarning")
    }
  })
}


# Read data ---------------------------------------------------------------

translations <- here::here("utilities", "vdfs.tsv") %>%
  readr::read_tsv(col_types = "ic")

vdfs <- read_tsv_helmet(
  here::here(config::get("helmet_data"), config::get("results"), "vehicle_kms_vdfs_areas.txt"),
  col_types = "cddddd"
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

readr::write_rds(vdfs, file = here::here("results", sprintf("vdfs_%s.rds", config::get("scenario"))))
