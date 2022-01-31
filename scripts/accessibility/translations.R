# -*- coding: utf-8-unix -*-

translations_areas <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc") %>%
  dplyr::filter(level != "helsinki_region")
levels_areas <- translations_areas$level
names(levels_areas) <- translations_areas$label

levels_age_groups <- c(
  "7-17 v" = "age_7-17",
  "18-29 v" = "age_18-29",
  "30-49 v" = "age_30-49",
  "50-64 v" = "age_50-64",
  "65-99 v" = "age_65-99"
)

levels_genders <- c("naiset" = "female",
                    "miehet" = "male")

translations_modes <- here::here("utilities", "modes.tsv") %>%
  readr::read_tsv(col_types = "cc")
levels_modes <- translations_modes$level
names(levels_modes) <- translations_modes$label
